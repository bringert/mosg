package se.chalmers.cs.mosg.client;

import com.google.gwt.user.client.ui.TreeItem;

public class InterpretationPanel extends TreeItem {

	private Semantics.Interpretation interpretation;

	private Reasoning.Answer consistency;
	private Reasoning.Answer informativity;
	private Reasoning.Answer answer;

	public InterpretationPanel(Semantics.Interpretation interpretation) {
		this.interpretation = interpretation;
		setText(interpretation.show());
	}

	public Semantics.Interpretation getInterpretation() {
		return interpretation;
	}

	public Reasoning.Answer getConsistency() {
		return consistency;
	}

	public Reasoning.Answer getInformativity() {
		return informativity;
	}

	public Reasoning.Answer getAnswer() {
		return answer;
	}

	public boolean isConsistent() {
		return checkAnswer(consistency);
	}

	public boolean isInformative() {
		return checkAnswer(informativity);
	}

	private static boolean checkAnswer(Reasoning.Answer a) {		
		return a != null && a.isYes();
	}

	private ParseResultPanel getParent() {
		return (ParseResultPanel)getParentItem();
	}

	public void setConsistency(Reasoning.Answer consistency) {
		this.consistency = consistency;		
		addItem("Consistent: " + showAnswer(consistency));
		setState(true);
		getParent().childConsistencyChecked();
	}

	public void setInformativity(Reasoning.Answer informativity) {
		this.informativity = informativity;
		addItem("Informative: " + showAnswer(informativity));
		setState(true);
		getParent().childInformativityChecked();
	}

	public void setAnswer(Reasoning.Answer answer) {
		this.answer = answer;
		addItem("Answer: " + showAnswer(answer));
		setState(true);
		getParent().childAnswerChecked();
	}

	private static String showAnswer(Reasoning.Answer a) {
		return a == null ? "Failed" : a.show();
	}

}
