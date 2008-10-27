package se.chalmers.cs.mosg.client;

import java.util.Iterator;

import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.DisclosureEvent;
import com.google.gwt.user.client.ui.DisclosureHandler;
import com.google.gwt.user.client.ui.DisclosurePanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.ScrollPanel;
import com.google.gwt.user.client.ui.SimplePanel;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class ScrollingDisclosurePanel extends Composite implements HasWidgets {

	private VerticalPanel contentPanel;

	public ScrollingDisclosurePanel(final String header) {
		contentPanel = new VerticalPanel();
		contentPanel.setStyleName("my-content");
		contentPanel.setHorizontalAlignment(VerticalPanel.ALIGN_LEFT);		
		ScrollPanel scrollPanel = new ScrollPanel(contentPanel);
		scrollPanel.setStyleName("my-scroll");
		final DisclosurePanel disclosurePanel = new DisclosurePanel(header, true);
		disclosurePanel.getHeader().setTitle("Hide " + header);
		disclosurePanel.setContent(scrollPanel);
		disclosurePanel.addEventHandler(new DisclosureHandler() {
			public void onClose(DisclosureEvent e) {
				disclosurePanel.getHeader().setTitle("Show " + header);
			}
			public void onOpen(DisclosureEvent e) {
				disclosurePanel.getHeader().setTitle("Hide " + header);
			}
		});
		Panel rootPanel = new SimplePanel();
		rootPanel.add(disclosurePanel);
		initWidget(rootPanel);
		setStyleName("my-ScrollingDisclosurePanel");
	}

	public void add(Widget w) {
		contentPanel.add(w);
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
