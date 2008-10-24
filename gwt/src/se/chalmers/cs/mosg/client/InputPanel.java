package se.chalmers.cs.mosg.client;

import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Tree;
import com.google.gwt.user.client.ui.TreeItem;

public class InputPanel extends Composite {

	private TreeItem treeItem;
	
	public InputPanel(String input) {
		treeItem = new TreeItem(input);
		Tree tree = new Tree();
		tree.addItem(treeItem);
		initWidget(tree);
		setStyleName("my-InputPanel");
	}
	
	public void setExpanded(boolean expanded) {
		treeItem.setState(expanded);
	}
	
	public InputTreePanel addInputTree(String inputTree) {
		InputTreePanel panel = new InputTreePanel(inputTree);
		treeItem.addItem(panel);
		treeItem.setState(true);
		return panel;
	}

}
