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
import com.google.gwt.user.client.ui.PopupPanel;
import com.google.gwt.user.client.ui.RootPanel;
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
	private VerticalPanel inputListPanel;
	private FactsBox factsBox;
	private PopupPanel statusPopup;
	private Label statusLabel;

	private void submit() {
		parse();
	}

	private void parse() {
		String text = suggest.getText();
		List<String> fromLangs = fromLangBox.getSelectedValues();
		setStatus("Parsing...");
		final InputPanel inputPanel = new InputPanel(text);
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

		setStatus("Interpreting...");
		for (PGF.ParseResult r : results.iterable()) {
			GWT.log("Interpreting " + r.getTree(), null);
			final InputTreePanel inputTreePanel = inputPanel.addInputTree(r.getTree());
			semantics.interpret(r.getTree(), new Semantics.Callback() {
				public void onResult (Semantics.Interpretations interpretations) {
					reason(interpretations, inputTreePanel);
				}
				public void onError (Throwable e) {
					showError("Interpretation failed", e);
				}
			});
		}
	}

	private void reason(Semantics.Interpretations interpretations, InputTreePanel inputTreePanel) {
		setStatus("Reasoning...");

		// Add interpretations to history
		for (Semantics.Interpretation i : interpretations.getInterpretations().iterable()) {
			InterpretationPanel panel = inputTreePanel.addInterpretation(i);
			if (i.isStatement()) {
				checkConsistency(i.getProposition(), panel);
				checkInformativity(i.getProposition(), panel);
			} else if (i.isYesNoQuestion()) {
				checkAnswer(i.getProposition(), panel);				
			}
		}

		// FIXME: wait for all the checks
		// FIXME: get consistent and informative facts
		// FIXME: optimistic/pessimistic/interactive
		// FIXME: add facts
		
		clearStatus();
	}
	
	private void checkConsistency(String fact, final InterpretationPanel panel) {
		GWT.log("Checking consistency " + fact, null);
		reasoning.isConsistent(getFacts(), fact, new Reasoning.Callback() {
			public void onResult (Reasoning.Answer answer) {
				panel.setConsistency(answer.show());
			}
			public void onError (Throwable e) {
				showError("Consistency check failed", e);
			}
		});
	}
	
	private void checkInformativity(String fact, final InterpretationPanel panel) {
		GWT.log("Checking informativity " + fact, null);
		reasoning.isInformative(getFacts(), fact, new Reasoning.Callback() {
			public void onResult (Reasoning.Answer answer) {
				panel.setInformativity(answer.show());
			}
			public void onError (Throwable e) {
				showError("Informativity check failed", e);
			}
		});
	}
	
	private void checkAnswer(String conjecture, final InterpretationPanel panel) {
		GWT.log("Checking answer " + conjecture, null);
		reasoning.isTrue(getFacts(), conjecture, new Reasoning.Callback() {
			public void onResult (Reasoning.Answer answer) {
				panel.setAnswer(answer.show());
			}
			public void onError (Throwable e) {
				showError("Answer check failed", e);
			}
		});
	}
	
	private void addFacts(List<String> facts) {
		for (String fact : facts) {
			factsBox.addFact(fact);
		}
	}
	
	private List<String> getFacts() {
		return factsBox.getFacts();
	}

	private void updateLangs() {
		oracle.setInputLangs(fromLangBox.getSelectedValues());
	}

	private void setStatus(String msg) {
		statusLabel.setText(msg);
		statusPopup.center();
	}

	private void showError(String msg, Throwable e) {
		GWT.log(msg, e);
		setStatus(msg);
	}

	private void clearStatus() {
		statusPopup.hide();
	}

	private void setGrammar(PGF.Grammar grammar) {
		fromLangBox.setGrammar(grammar);

		updateLangs();
		clearStatus();
		submitButton.setEnabled(true);
	}

	private void createMosgUI() {

		oracle = new CompletionOracle(pgf, new CompletionOracle.ErrorHandler() {
			public void onError(Throwable e) {
				showError("Completion failed", e);
			}
		});

		suggest = new SuggestBox(oracle);
		suggest.addKeyboardListener(new KeyboardListenerAdapter() {
			public void onKeyUp (Widget sender, char keyCode, int modifiers) {
				if (keyCode == KEY_ENTER) {
					submit();
				}
			}
		});

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
				submit();
			}
		});

		HorizontalPanel settingsPanel = new HorizontalPanel();
		settingsPanel.addStyleName("my-settingsPanel");
		settingsPanel.setVerticalAlignment(HorizontalPanel.ALIGN_MIDDLE);
		settingsPanel.add(new Label("From:"));
		settingsPanel.add(fromLangBox);
		settingsPanel.add(submitButton);

		factsBox = new FactsBox();
		
		inputListPanel = new VerticalPanel();
		inputListPanel.setHorizontalAlignment(VerticalPanel.ALIGN_LEFT);
		inputListPanel.setStyleName("my-inputListPanel");		
		
		VerticalPanel vPanel = new VerticalPanel();
		vPanel.setWidth("100%");
		vPanel.setHorizontalAlignment(VerticalPanel.ALIGN_CENTER);
		vPanel.add(suggest);
		vPanel.add(settingsPanel);
		vPanel.add(factsBox);
		vPanel.add(inputListPanel);

		RootPanel.get().add(vPanel);

	}

	public void onModuleLoad() {    
		statusLabel = new Label("Loading...");
		statusPopup = new PopupPanel(true, true);
		statusPopup.add(statusLabel);
		statusPopup.center();

		pgf = new PGF(pgfBaseURL, pgfName);
		semantics = new Semantics(semanticsBaseURL);
		reasoning = new Reasoning(reasoningBaseURL);

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
