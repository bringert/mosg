package se.chalmers.cs.mosg.client;

import se.chalmers.cs.gf.gwt.client.*;
import se.chalmers.cs.gf.gwt.client.JSONRequestBuilder.Arg;

import com.google.gwt.core.client.JavaScriptObject;

import java.util.List;
import java.util.ArrayList;

public class Reasoning {

	private String baseURL;

	public Reasoning (String baseURL) {
		this.baseURL = baseURL;
	}

	/* istrue */
	
	public JSONRequest isTrue (List<String> facts, String conjecture, final Callback callback) {
		return reason("istrue", facts, conjecture, callback);
	}
	
	/* isconsistent */
	
	public JSONRequest isConsistent (List<String> facts, String conjecture, final Callback callback) {
		return reason("isconsistent", facts, conjecture, callback);
	}
	
	/* isinformative */
	
	public JSONRequest isInformative (List<String> facts, String conjecture, final Callback callback) {
		return reason("isinformative", facts, conjecture, callback);
	}

	/* Common */

	public JSONRequest reason (String command, List<String> facts, String conjecture, final Callback callback) {
		List<Arg> args = new ArrayList<Arg>();
		if (facts != null) {
			for (String fact : facts) {
				args.add(new Arg("fact", fact));
			}
		}
		args.add(new Arg("conjecture", conjecture));
		return sendRequest(command, args, callback);
	}
	
	public interface Callback extends JSONCallback<Answer> {  
	}

	public static class Answer extends JavaScriptObject {
		protected Answer() { }

		public final native boolean isYes() /*-{ return this.answer == "yes"; }-*/;
		public final native boolean isNo() /*-{ return this.answer == "no"; }-*/;
		public final native boolean isUnknown() /*-{ return this.answer == "unknown"; }-*/;
		
		public final native String show() /*-{ return this.answer; }-*/;
	}
	
	public <T extends JavaScriptObject> JSONRequest sendRequest(String command, List<Arg> args, final JSONCallback<T> callback) {
		return JSONRequestBuilder.sendRequest(baseURL + "/" + command, args, callback);
	}

}