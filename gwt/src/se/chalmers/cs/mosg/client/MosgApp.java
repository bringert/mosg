package se.chalmers.cs.mosg.client;

import java.util.ArrayList;
import java.util.List;

import se.chalmers.cs.gf.gwt.client.PGF;
import se.chalmers.cs.gf.gwt.client.PGFWrapper;
import se.chalmers.cs.gf.gwt.client.SettingsPanel;
import se.chalmers.cs.gf.gwt.client.SuggestPanel;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.SimplePanel;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;


public class MosgApp implements EntryPoint {

	private static final String pgfBaseURL = "/pgf";
	private static final String semanticsBaseURL = "/semantics";
	private static final String reasoningBaseURL = "/reasoning";
	private static final String pgfName = "Syntax.pgf";

	private PGFWrapper pgf;
	private Semantics semantics;
	private Reasoning reasoning;

	private Widget mosgUI;
	
	private SuggestPanel suggestPanel;
	private ScrollingDisclosurePanel logPanel;
	private FactsBox factsBox;
	private SimplePanel statusPanel;
	private Label statusLabel;

	//
	// Parsing
	//

	private void parse() {
		String text = massageInput(suggestPanel.getText());
		setStatus("Parsing...");
		final InputPanel inputPanel = new InputPanel(text);
		inputPanel.addReasoningListener(new InputPanel.ReasoningListener() {
			public void onReasoningDone(List<InterpretationPanel> interpretations) {
				reasoningDone(interpretations);
			}
		});
		logPanel.add(inputPanel);
		pgf.parse(text, new PGF.ParseCallback() {
			public void onResult(PGF.ParseResults results) {
				interpret(results, inputPanel);
			}
			public void onError(Throwable e) {
				GWT.log("Parsing failed", e);
				inputPanel.parseFailed();
			}
		});
	}

	/** Does some preprocessing of the string before sending it to the server. */
	private String massageInput(String input) {
		return input.toLowerCase();
	}

	//
	// Interpretation
	//

	private void interpret(PGF.ParseResults results, InputPanel inputPanel) {
		for (Widget w : logPanel) {
			InputPanel p = (InputPanel)w;
			p.setExpanded(false);
		}

		if (results.isEmpty()) {
			showError("No parse results.", null);
			inputPanel.parseFailed();
		} else {
			for (PGF.ParseResult r : results.iterable()) {
				setStatus("Interpreting...");
				final ParseResultPanel parseResultPanel = inputPanel.addInputTree(r);
				semantics.interpret(r.getTree(), new Semantics.Callback() {
					public void onResult (Semantics.Interpretations interpretations) {
						reason(interpretations, parseResultPanel);
					}
					public void onError (Throwable e) {
						GWT.log("Interpretation failed", e);
						parseResultPanel.interpretationFailed(e.getMessage());
					}
				});
			}
		}
	}

	//
	// Reasoning
	//

	private void reason(Semantics.Interpretations interpretations, ParseResultPanel parseResultPanel) {

		if (interpretations.isEmpty()) {
			parseResultPanel.interpretationFailed(interpretations.getError());
			return;		
		}

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
				GWT.log("Consistency check failed", e);
				panel.setConsistency(null);
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
				GWT.log("Informativity check failed", e);
				panel.setInformativity(null);
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
				GWT.log("Answer check failed", e);
				panel.setAnswer(null);
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

		suggestPanel.setText("");
	}

	private void showAnswer(String answer) {
		setStatus(answer);
	}

	//
	// Facts
	//

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

	//
	// Status
	//

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
	
	//
	// Settings
	//

	protected class MySettingsListener extends PGFWrapper.SettingsAdapter {
		// Will only happen on load
		public void onAvailableGrammarsChanged() {
			if (pgf.getPGFName() == null) {
				List<String> grammars = pgf.getGrammars();
				if (!grammars.isEmpty()) {
					pgf.setPGFName(grammars.get(0));
				}
			}			
			RootPanel.get().clear();
			RootPanel.get().add(mosgUI);
		}
		public void onAvailableLanguagesChanged() {
			if (pgf.getInputLanguage() == null) {
				GWT.log("Setting input language to user language: " + pgf.getUserLanguage(), null);
				pgf.setInputLanguage(pgf.getUserLanguage());
			}
		}
		public void onSettingsError(String msg, Throwable e) {
			showError(msg,e);
		}
	}
	
	//
	// GUI
	//

	private Widget createMosgUI() {

		statusLabel = new Label();
		statusPanel = new SimplePanel();
		statusPanel.setStylePrimaryName("my-statusPanel");
		statusPanel.add(statusLabel);

		suggestPanel = new SuggestPanel(pgf);
		suggestPanel.setLimit(10);
		suggestPanel.setTitle("Enter a statement or a question");
		suggestPanel.addSubmitListener(new SuggestPanel.SubmitListener() {
			public void onSubmit(String text) {
				parse();
			}
		});

		SettingsPanel settingsPanel = new SettingsPanel(pgf, false, false);

		factsBox = new FactsBox();

		logPanel = new ScrollingDisclosurePanel("Log");

		VerticalPanel vPanel = new VerticalPanel();
		vPanel.setWidth("100%");
		vPanel.setHorizontalAlignment(VerticalPanel.ALIGN_CENTER);
		vPanel.add(suggestPanel);
		vPanel.add(settingsPanel);
		vPanel.add(statusPanel);
		vPanel.add(factsBox);
		vPanel.add(logPanel);

		return vPanel;
	}
	
	protected Widget createLoadingWidget () {
		VerticalPanel loadingPanel = new VerticalPanel();
		loadingPanel.setHorizontalAlignment(VerticalPanel.ALIGN_CENTER);
		loadingPanel.add(new Label("Loading..."));
		return loadingPanel;
	}

	//
	// Initialization
	//

	public void onModuleLoad() {
		RootPanel.get().add(createLoadingWidget());
		
		pgf = new PGFWrapper(new PGF(pgfBaseURL), new MySettingsListener());
		semantics = new Semantics(semanticsBaseURL);
		reasoning = new Reasoning(reasoningBaseURL);

		mosgUI = createMosgUI();
		
		pgf.setPGFName(pgfName);
	}

}
