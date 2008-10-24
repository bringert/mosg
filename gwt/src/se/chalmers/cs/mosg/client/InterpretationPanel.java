package se.chalmers.cs.mosg.client;

import com.google.gwt.user.client.ui.TreeItem;

public class InterpretationPanel extends TreeItem {

	public InterpretationPanel(Semantics.Interpretation interpretation) {
		setText(interpretation.show());
	}

	public void setConsistency(String consistency) {
		addItem("Consistent: " + consistency);
	}

	public void setInformativity(String informativity) {
		addItem("Informative: " + informativity);
	}
	
	public void setAnswer(String answer) {
		addItem("Answer: " + answer);
	}

	
}
