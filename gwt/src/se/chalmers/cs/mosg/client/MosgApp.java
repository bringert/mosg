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
	private PGF.Grammar grammar;
	private InputLanguageBox fromLangBox;
	private Button submitButton;
	private FactsBox factsBox;
	private PopupPanel statusPopup;
	private Label statusLabel;

	private void addFact(String fact) {
		factsBox.addFact(fact);
	}

	private void submit() {
		parse();
	}

	private void parse() {
		setStatus("Parsing...");
		pgf.parse(suggest.getText(), fromLangBox.getSelectedValues(), null, 
				new PGF.ParseCallback() {
			public void onResult (PGF.ParseResults results) {
				interpret(results);
			}
			public void onError (Throwable e) {
				showError("Parsing failed", e);
			}
		});
	}

	private void interpret(PGF.ParseResults results) {
		setStatus("Interpreting...");
		for (PGF.ParseResult r : results.iterable()) {
			GWT.log("Interpreting " + r.getTree(), null);
			semantics.interpret(r.getTree(), 
					new Semantics.InterpretCallback() {
				public void onResult (Semantics.Interpretations interpretations) {
					reason(interpretations);
				}
				public void onError (Throwable e) {
					showError("Interpretation failed", e);
				}
			});
		}
	}

	private void reason(Semantics.Interpretations interpretations) {
		setStatus("Reasoning...");
		for (Semantics.Interpretation i : interpretations.getInterpretations().iterable()) {
			if (i.isStatement()) {
				// FIXME: what if there are multiple interpretations?
				addFact(i.getProp());
			} else if (i.isYesNoQuestion()) {
				GWT.log("Checking " + i.getProp(), null);
				reasoning.isTrue(getFacts(), i.getProp(), 
						new Reasoning.ReasoningCallback() {
					public void onResult (Reasoning.Answer answer) {
						if (answer.isYes()) {
							setStatus("Yes");
						} else if (answer.isNo()) {
							setStatus("No");
						} else {
							setStatus("Unknown");
						}
					}
					public void onError (Throwable e) {
						showError("Reasoning failed", e);
					}
				});
			}
		}
		clearStatus();
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
		this.grammar = grammar;

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

		VerticalPanel vPanel = new VerticalPanel();
		vPanel.setWidth("100%");
		vPanel.setHorizontalAlignment(VerticalPanel.ALIGN_CENTER);
		vPanel.add(suggest);
		vPanel.add(settingsPanel);
		vPanel.add(factsBox);

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
