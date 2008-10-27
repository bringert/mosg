package se.chalmers.cs.mosg.client;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.Widget;

public class FactsBox extends ScrollingDisclosurePanel {

	public FactsBox() {
		super("Facts");
		addStyleName("my-FactsBox");		
	}
	
	public void addFact(String fact) {
		CheckBox c = new CheckBox(fact);
		c.setChecked(true);
		add(c);
	}
	
	public List<String> getFacts() {
		List<String> ret = new ArrayList<String>();
		for (Widget w : this) {
			CheckBox c = (CheckBox)w;
			if (c.isChecked()) {
				ret.add(c.getText());
			}
		}
		return ret;
	}
	
}
