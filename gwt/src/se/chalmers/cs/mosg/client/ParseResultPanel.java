package se.chalmers.cs.mosg.client;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import se.chalmers.cs.gf.gwt.client.PGF;

import com.google.gwt.user.client.ui.TreeItem;

public class ParseResultPanel extends TreeItem {

	private PGF.ParseResult parseResult;

	/** The number of statement children whose consistency has not been checked. */
	private int consistencyUnchecked = 0;

	/** The number of statement children whose informativity has not been checked. */
	private int informativityUnchecked = 0;

	/** The number of yes/no question children whose answer has not been checked. */
	private int answerUnchecked = 0;
	
	private List<InterpretationPanel> interpretations = new ArrayList<InterpretationPanel>();

	private List<ReasoningListener> listeners = new LinkedList<ReasoningListener>();

	public ParseResultPanel(PGF.ParseResult parseResult) {
		this.parseResult = parseResult;
		setText(parseResult.getTree());
		setStyleName("my-InputTreePanel");
	}

	public PGF.ParseResult getParseResult() {
		return parseResult;
	}

	public InterpretationPanel addInterpretation(Semantics.Interpretation interpretation) {
		InterpretationPanel panel = new InterpretationPanel(interpretation);
		interpretations.add(panel);
		addItem(panel);
		if (interpretation.isStatement()) {
			consistencyUnchecked++;
			informativityUnchecked++;
		} else if (interpretation.isYesNoQuestion()) {
			answerUnchecked++;			
		}
		setState(true);
		return panel;
	}

	public void interpretationFailed (String error) {
		addItem("Interpretation failed: " + error);
		fireReasoningDone();
	}

	public void childConsistencyChecked() {
		consistencyUnchecked--;
		fireReasoningDone();
	}

	public void childInformativityChecked() {
		informativityUnchecked--;
		fireReasoningDone();
	}

	public void childAnswerChecked() {
		answerUnchecked--;
		fireReasoningDone();
	}

	private boolean areAllChecked() {
		return consistencyUnchecked == 0 && informativityUnchecked == 0 && answerUnchecked == 0;
	}

	public void addReasoningListener(ReasoningListener l) {
		listeners.add(l);
	}

	private void fireReasoningDone() {
		if (areAllChecked()) {
			for (ReasoningListener l : listeners) {
				l.onReasoningDone(interpretations);
			}
		}
	}

	public interface ReasoningListener {	
		public void onReasoningDone(List<InterpretationPanel> interpretations);
	}

}

