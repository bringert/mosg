package se.chalmers.cs.mosg.client;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import se.chalmers.cs.gf.gwt.client.PGF;

import com.google.gwt.user.client.ui.TreeItem;

public class ParseResultPanel extends TreeItem {

	private PGF.ParseResult parseResult;
	
	private int unchecked = 0;
	
	private List<InterpretationPanel> interpretations = new ArrayList<InterpretationPanel>();

	private List<ReasoningListener> listeners = new LinkedList<ReasoningListener>();

	public ParseResultPanel(PGF.ParseResult parseResult) {
		this.parseResult = parseResult;
		setText(parseResult.getTree());
		setStyleName("my-ParseResultPanel");
		addStyleDependentName("incomplete");
	}

	public PGF.ParseResult getParseResult() {
		return parseResult;
	}

	public InterpretationPanel addInterpretation(Semantics.Interpretation interpretation) {
		InterpretationPanel panel = new InterpretationPanel(interpretation);
		panel.addReasoningListener(childListener);
		interpretations.add(panel);
		addItem(panel);
		setState(true);
		unchecked++;
		return panel;
	}
	
	private InterpretationPanel.ReasoningListener childListener = new InterpretationPanel.ReasoningListener() {
		public void onReasoningDone(InterpretationPanel interpretationPanel) {
			unchecked--;
			fireReasoningDone();
		}
	};

	public void interpretationFailed (String error) {
		addItem("Interpretation failed: " + error);
		removeStyleDependentName("incomplete");
		addStyleDependentName("failed");
		fireReasoningDone();
	}

	public void addReasoningListener(ReasoningListener l) {
		listeners.add(l);
	}

	private void fireReasoningDone() {
		if (unchecked == 0) {
			removeStyleDependentName("incomplete");
			for (ReasoningListener l : listeners) {
				l.onReasoningDone(interpretations);
			}
		}
	}

	public interface ReasoningListener {	
		public void onReasoningDone(List<InterpretationPanel> interpretations);
	}

}

