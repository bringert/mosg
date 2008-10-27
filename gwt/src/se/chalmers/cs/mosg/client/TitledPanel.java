package se.chalmers.cs.mosg.client;

import java.util.Iterator;

import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.DockPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class TitledPanel extends Composite implements HasWidgets {

	private Label titleLabel;
	private Panel contentPanel;

	public TitledPanel(String title) {
		this.titleLabel = new Label(title);
		this.contentPanel = new VerticalPanel();
		DockPanel rootPanel = new DockPanel();		
		rootPanel.add(titleLabel, DockPanel.NORTH);
		rootPanel.add(contentPanel, DockPanel.CENTER);
		initWidget(rootPanel);
	}

	public String getTitle() {
		return titleLabel.getText();
	}

	public void setTitle(String title) {
		titleLabel.setText(title);
	}

	public void add(Widget child) {
		contentPanel.add(child);
	}

	public void clear() {
		contentPanel.clear();
	}

	public Iterator<Widget> iterator() {
		return contentPanel.iterator();
	}

	public boolean remove(Widget w) {
		return contentPanel.remove(w);
	}

}
