package se.chalmers.cs.mosg.client;

import java.util.ArrayList;
import java.util.List;

import se.chalmers.cs.gf.gwt.client.CompletionOracle;
import se.chalmers.cs.gf.gwt.client.InputLanguageBox;
import se.chalmers.cs.gf.gwt.client.PGF;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.ChangeListener;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.KeyboardListenerAdapter;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.SimplePanel;
import com.google.gwt.user.client.ui.SuggestBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;


public class MosgApp implements EntryPoint {

	private static final String pgfBaseURL = "/pgf";
	private static final String semanticsBaseURL = "/semantics";
	private static final String reasoningBaseURL = "/reasoning";
	private static final String pgfName = "Syntax.pgf";

	private PGF pgf;
	private Semantics semantics;
	private Reasoning reasoning;

	private CompletionOracle oracle;
	private SuggestBox suggest;
	private InputLanguageBox fromLangBox;
	private Button submitButton;
	private ScrollingDisclosurePanel inputListPanel;
	private FactsBox factsBox;
	private SimplePanel statusPanel;
	private Label statusLabel;


	private void parse() {
		String text = suggest.getText();
		List<String> fromLangs = fromLangBox.getSelectedValues();
		setStatus("Parsing...");
		final InputPanel inputPanel = new InputPanel(text);
		inputPanel.addReasoningListener(new InputPanel.ReasoningListener() {
			public void onReasoningDone(List<InterpretationPanel> interpretations) {
				reasoningDone(interpretations);
			}
		});
		inputListPanel.add(inputPanel);
		pgf.parse(text, fromLangs, null, new PGF.ParseCallback() {
			public void onResult(PGF.ParseResults results) {
				interpret(results, inputPanel);
			}
			public void onError(Throwable e) {
				showError("Parsing failed", e);
			}
		});
	}

	private void interpret(PGF.ParseResults results, InputPanel inputPanel) {
		for (Widget w : inputListPanel) {
			InputPanel p = (InputPanel)w;
			p.setExpanded(false);
		}

		if (results.isEmpty()) {
			showError("No parse results.", null);
		} else {
			for (PGF.ParseResult r : results.iterable()) {
				setStatus("Interpreting...");
				final ParseResultPanel parseResultPanel = inputPanel.addInputTree(r);
				semantics.interpret(r.getTree(), new Semantics.Callback() {
					public void onResult (Semantics.Interpretations interpretations) {
						reason(interpretations, parseResultPanel);
					}
					public void onError (Throwable e) {
						showError("Interpretation failed", e);
					}
				});
			}
		}
	}

	private void reason(Semantics.Interpretations interpretations, ParseResultPanel parseResultPanel) {

		if (interpretations.isEmpty()) {
			showError("No interpretations.", null);
			parseResultPanel.interpretationFailed(interpretations.getError());
			return;		
		}

		setStatus("Checking interpretations...");
		for (Semantics.Interpretation i : interpretations.getInterpretations().iterable()) {
			InterpretationPanel panel = parseResultPanel.addInterpretation(i);
			if (i.isStatement()) {
				checkConsistency(i.getProposition(), panel);
				checkInformativity(i.getProposition(), panel);
			} else if (i.isYesNoQuestion()) {
				checkAnswer(i.getProposition(), panel);				
			}
		}
	}

	private void checkConsistency(String fact, final InterpretationPanel panel) {
		GWT.log("Checking consistency " + fact, null);
		reasoning.isConsistent(getFacts(), fact, new Reasoning.Callback() {
			public void onResult (Reasoning.Answer answer) {
				panel.setConsistency(answer);
			}
			public void onError (Throwable e) {
				panel.setConsistency(null);
				showError("Consistency check failed", e);
			}
		});
	}

	private void checkInformativity(String fact, final InterpretationPanel panel) {
		GWT.log("Checking informativity " + fact, null);
		reasoning.isInformative(getFacts(), fact, new Reasoning.Callback() {
			public void onResult (Reasoning.Answer answer) {
				panel.setInformativity(answer);
			}
			public void onError (Throwable e) {
				panel.setInformativity(null);
				showError("Informativity check failed", e);
			}
		});
	}

	private void checkAnswer(String conjecture, final InterpretationPanel panel) {
		GWT.log("Checking answer " + conjecture, null);
		reasoning.isTrue(getFacts(), conjecture, new Reasoning.Callback() {
			public void onResult (Reasoning.Answer answer) {
				panel.setAnswer(answer);
			}
			public void onError (Throwable e) {
				panel.setAnswer(null);
				showError("Answer check failed", e);
			}
		});
	}

	private void reasoningDone(List<InterpretationPanel> interpretations) {
		if (interpretations.isEmpty()) {
			setStatus("No interpretations.");
			return;
		}

		clearStatus();
		List<String> okStatements = new ArrayList<String>();
		List<Reasoning.Answer> yesNoAnswers = new ArrayList<Reasoning.Answer>();

		for (InterpretationPanel i : interpretations) {
			if (i.getInterpretation().isStatement() && i.isConsistent() && i.isInformative()) {
				okStatements.add(i.getInterpretation().getProposition());
			} else if (i.getInterpretation().isYesNoQuestion()) {
				yesNoAnswers.add(i.getAnswer());
			}
		}

		if (yesNoAnswers.isEmpty()) {
			if (okStatements.isEmpty()) {
				setStatus("No consistent and informative interpretations.");
				return;
			} else {
				String fact = combineFacts(okStatements);
				addFact(fact);			
			}
		} else {
			if (okStatements.isEmpty()) {
				boolean someYes = false;
				boolean someNo = false;
				for (Reasoning.Answer a : yesNoAnswers) {
					if (a.isYes()) {
						someYes = true;
					} else if (a.isNo()) {
						someNo = true;
					}
				}
				if (someYes && !someNo) {
					showAnswer("Yes.");
				} else if (!someYes && someNo) {
					showAnswer("No.");
				} else if (!someYes && !someNo) {
					showAnswer("Unknown.");
				} else {
					showError("Inconsistent answers.", null);
				}
			} else {
				showError("Input is ambiguous between statement and question.", null);
				return;
			}
		}

		suggest.setText("");
	}
	
	private void showAnswer(String answer) {
		setStatus(answer);
	}

	private String combineFacts(List<String> facts) {
		// FIXME: this is pessimistic, implement the others too
		StringBuilder sb = new StringBuilder();
		for (String fact : facts) {
			if (sb.length() > 0)
				sb.append(" | ");
			sb.append('(').append(fact).append(')');
		}
		return sb.toString();
	}

	private void addFact(String fact) {
		factsBox.addFact(fact);
	}

	private List<String> getFacts() {
		return factsBox.getFacts();
	}

	private void updateLangs() {
		oracle.setInputLangs(fromLangBox.getSelectedValues());
	}

	private void setStatus(String msg) {
		statusPanel.removeStyleDependentName("error");
		statusLabel.setText(msg);
	}

	private void showError(String msg, Throwable e) {
		GWT.log(msg, e);
		statusPanel.addStyleDependentName("error");
		statusLabel.setText(msg);
	}

	private void clearStatus() {
		statusPanel.removeStyleDependentName("error");
		statusLabel.setText("");
	}

	private void setGrammar(PGF.Grammar grammar) {
		fromLangBox.setGrammar(grammar);

		updateLangs();
		clearStatus();
		submitButton.setEnabled(true);
	}

	private void createMosgUI() {
		
		statusLabel = new Label("Loading...");
		statusPanel = new SimplePanel();
		statusPanel.setStylePrimaryName("my-statusPanel");
		statusPanel.add(statusLabel);

		suggest = new SuggestBox(oracle);
		suggest.setLimit(10);
		suggest.setTitle("Enter a statement or a question");
		suggest.addKeyboardListener(new KeyboardListenerAdapter() {
			public void onKeyUp (Widget sender, char keyCode, int modifiers) {
				if (keyCode == KEY_ENTER) {
					parse();
				}
			}
		});
		SimplePanel suggestPanel = new SimplePanel() {
			public void onLoad() {
				suggest.setFocus(true);
			}
		};
		suggestPanel.setStyleName("my-suggestPanel");
		suggestPanel.add(suggest);

		fromLangBox = new InputLanguageBox();
		fromLangBox.setEnabled(false);
		fromLangBox.addItem("Any language", "");
		fromLangBox.addChangeListener(new ChangeListener() {
			public void onChange(Widget sender) {
				updateLangs();
			}
		});

		submitButton = new Button("Submit");
		submitButton.setEnabled(false);
		submitButton.addClickListener(new ClickListener() {
			public void onClick(Widget sender) {
				parse();
			}
		});

		HorizontalPanel settingsPanel = new HorizontalPanel();
		settingsPanel.addStyleName("my-settingsPanel");
		settingsPanel.setVerticalAlignment(HorizontalPanel.ALIGN_MIDDLE);
		settingsPanel.add(new Label("Language:"));
		settingsPanel.add(fromLangBox);
		settingsPanel.add(submitButton);

		factsBox = new FactsBox();

		inputListPanel = new ScrollingDisclosurePanel("Log");

		VerticalPanel vPanel = new VerticalPanel();
		vPanel.setWidth("100%");
		vPanel.setHorizontalAlignment(VerticalPanel.ALIGN_CENTER);
		vPanel.add(suggestPanel);
		vPanel.add(settingsPanel);
		vPanel.add(statusPanel);
		vPanel.add(factsBox);
		vPanel.add(inputListPanel);

		RootPanel.get().add(vPanel);

	}

	public void onModuleLoad() {
		pgf = new PGF(pgfBaseURL, pgfName);
		semantics = new Semantics(semanticsBaseURL);
		reasoning = new Reasoning(reasoningBaseURL);

		oracle = new CompletionOracle(pgf, new CompletionOracle.ErrorHandler() {
			public void onError(Throwable e) {
				showError("Completion failed", e);
			}
		});
		
		createMosgUI();

		pgf.grammar(new PGF.GrammarCallback() {
			public void onResult(PGF.Grammar grammar) {
				setGrammar(grammar);
			}

			public void onError (Throwable e) {
				showError("Error getting language information", e);
			}
		});	
	}

}
