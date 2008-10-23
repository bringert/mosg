package se.chalmers.cs.mosg.client;

import se.chalmers.cs.gf.gwt.client.*;

import com.google.gwt.core.client.JavaScriptObject;

import java.util.List;
import java.util.ArrayList;

public class Reasoning {

	private String baseURL;

	public Reasoning (String baseURL) {
		this.baseURL = baseURL;
	}

	/* istrue */
	
	public JSONRequest isTrue (List<String> facts, String conjecture, final ReasoningCallback callback) {
		List<Arg> args = new ArrayList<Arg>();
		if (facts != null) {
			for (String fact : facts) {
				args.add(new Arg("fact", fact));
			}
		}
		args.add(new Arg("conjecture", conjecture));
		return sendRequest("istrue", args, callback);
	}

	public interface ReasoningCallback extends JSONCallback<Answer> {  
	}

	public static class Answer extends JavaScriptObject {
		protected Answer() { }

		public final native String getAnswer() /*-{ return this.answer; }-*/;
	}

	/* Common */

	public <T extends JavaScriptObject> JSONRequest sendRequest(String command, List<Arg> args, final JSONCallback<T> callback) {
		return JSONRequestBuilder.sendRequest(baseURL + "/" + command, args, callback);
	}

}