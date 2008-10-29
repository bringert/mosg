package se.chalmers.cs.mosg.client;

import java.util.LinkedList;
import java.util.List;

import com.google.gwt.user.client.ui.TreeItem;

public class InterpretationPanel extends TreeItem {

	private Semantics.Interpretation interpretation;

	private AnswerPanel consistencyPanel = null;
	private AnswerPanel informativityPanel = null;
	private AnswerPanel answerPanel = null;
	
	private List<ReasoningListener> listeners = new LinkedList<ReasoningListener>();
	
	public InterpretationPanel(Semantics.Interpretation interpretation) {
		this.interpretation = interpretation;
		setText(interpretation.show());
		setStyleName("my-InterpretationPanel");
		addStyleDependentName("incomplete");
		if (interpretation.isStatement()) {
			addItem(this.consistencyPanel = new AnswerPanel("Consistent"));
			addItem(this.informativityPanel = new AnswerPanel("Informative"));
		} else if (interpretation.isYesNoQuestion()) {
			addItem(this.answerPanel = new AnswerPanel("Answer"));
		}
	}

	public Semantics.Interpretation getInterpretation() {
		return interpretation;
	}

	public Reasoning.Answer getConsistency() {
		return consistencyPanel.getAnswer();
	}

	public Reasoning.Answer getInformativity() {
		return informativityPanel.getAnswer();
	}

	public Reasoning.Answer getAnswer() {
		return answerPanel.getAnswer();
	}

	public boolean isConsistent() {
		return checkAnswer(getConsistency());
	}

	public boolean isInformative() {
		return checkAnswer(getInformativity());
	}

	private static boolean checkAnswer(Reasoning.Answer a) {		
		return a != null && a.isYes();
	}

	public void setConsistency(Reasoning.Answer answer) {
		consistencyPanel.setAnswer(answer);
	}

	public void setInformativity(Reasoning.Answer answer) {
		informativityPanel.setAnswer(answer);
	}

	public void setAnswer(Reasoning.Answer answer) {
		answerPanel.setAnswer(answer);
	}
	
	public void addReasoningListener(ReasoningListener l) {
		listeners.add(l);
	}

	public void fireReasoningDone () {
		if (isDone(consistencyPanel) && isDone(answerPanel) && isDone(informativityPanel)) {
			removeStyleDependentName("incomplete");
			for (ReasoningListener l : listeners) {
				l.onReasoningDone(this);
			}
		}
	}
	
	private static boolean isDone(AnswerPanel answerPanel) {
		return answerPanel == null || answerPanel.isChecked();
	}

	public interface ReasoningListener {	
		public void onReasoningDone(InterpretationPanel interpretationPanel);
	}
	
	private class AnswerPanel extends TreeItem {

		private String text;
		
		private Reasoning.Answer answer = null;
		
		private boolean checked = false;

		public AnswerPanel(String text) {
			this.text = text;
			setStyleName("my-AnswerPanel");
			addStyleDependentName("incomplete");
			setText(text + ": ?");
		}
		
		public Reasoning.Answer getAnswer() {
			return answer;
		}

		public boolean isChecked() {
			return checked;
		}

		public void setAnswer(Reasoning.Answer answer) {
			this.answer = answer;
			removeStyleDependentName("incomplete");
			if (answer == null) {
				setText(text + ": Failed");
				addStyleDependentName("failed");
			} else {
				setText(text + ": " + answer.show());			
			}
			checked = true;
			InterpretationPanel.this.setState(true);
			fireReasoningDone();
		}
		
	}
	
}
