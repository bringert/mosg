package se.chalmers.cs.mosg.client;

import com.google.gwt.user.client.ui.TreeItem;

public class InputTreePanel extends TreeItem {

	public InputTreePanel(String inputTree) {
		super(inputTree);
		setStyleName("my-InputTreePanel");
	}

	public InterpretationPanel addInterpretation(Semantics.Interpretation interpretation) {
		InterpretationPanel panel = new InterpretationPanel(interpretation);
		addItem(panel);
		setState(true);
		return panel;
	}

}
