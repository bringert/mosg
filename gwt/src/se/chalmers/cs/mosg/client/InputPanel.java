package se.chalmers.cs.mosg.client;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import se.chalmers.cs.gf.gwt.client.PGF;

import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Tree;
import com.google.gwt.user.client.ui.TreeItem;

public class InputPanel extends Composite {

	private TreeItem treeItem;

	/** The number of children what have not been completely checked
	 *  for consistency and informativity, or answer. */
	private int unchecked;

	private List<InterpretationPanel> interpretations = new ArrayList<InterpretationPanel>();

	private List<ReasoningListener> listeners = new LinkedList<ReasoningListener>();

	public InputPanel(String input) {
		treeItem = new TreeItem(input);
		Tree tree = new Tree();
		tree.addItem(treeItem);
		initWidget(tree);
		setStyleName("my-InputPanel");
		addStyleDependentName("incomplete");
	}

	public void setExpanded(boolean expanded) {
		treeItem.setState(expanded);
	}

	public ParseResultPanel addInputTree(PGF.ParseResult inputTree) {
		unchecked++;
		ParseResultPanel panel = new ParseResultPanel(inputTree);
		panel.addReasoningListener(childListener);
		treeItem.addItem(panel);
		treeItem.setState(true);
		return panel;
	}
	
	private ParseResultPanel.ReasoningListener childListener = new ParseResultPanel.ReasoningListener() {
		public void onReasoningDone(List<InterpretationPanel> childInterpretations) {
			interpretations.addAll(childInterpretations);
			unchecked--;
			fireReasoningDone();
		}
	};
	
	public void parseFailed() {
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
