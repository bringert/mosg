package se.chalmers.cs.mosg.client;

import se.chalmers.cs.gf.gwt.client.*;
import se.chalmers.cs.gf.gwt.client.JSONRequestBuilder.Arg;

import com.google.gwt.core.client.JavaScriptObject;

import java.util.List;
import java.util.ArrayList;

public class Semantics {

	private String baseURL;

	public Semantics (String baseURL) {
		this.baseURL = baseURL;
	}

	/* Interpretation */

	public JSONRequest interpret (String tree, final Callback callback) {
		List<Arg> args = new ArrayList<Arg>();
		args.add(new Arg("tree", tree));
		return sendRequest("interpret", args, callback);
	}

	public interface Callback extends JSONCallback<Interpretations> {  
	}

	public static class Interpretations extends JavaScriptObject {
		protected Interpretations() { }

		public final boolean isEmpty() {
			return getInterpretations().isEmpty();
		}

		public final boolean onlyStatements() {
			for (Interpretation i : getInterpretations().iterable()) {
				if (!i.isStatement()) { return false; }
			}
			return true;
		}

		public final boolean onlyYesNoQuestions() {
			for (Interpretation i : getInterpretations().iterable()) {
				if (!i.isYesNoQuestion()) { return false; }
			}
			return true;
		}

		public final native String getError() /*-{ return this.error; }-*/;

		public final native IterableJsArray<Interpretation> getInterpretations() /*-{ return this.interpretations; }-*/;
	}

	public static class Interpretation extends JavaScriptObject {
		protected Interpretation() { }

		public final native boolean isStatement() /*-{ return this.type == "stm"; }-*/;
		public final native boolean isYesNoQuestion() /*-{ return this.type == "ynquest"; }-*/;
		public final native String getProposition() /*-{ return this.prop; }-*/;
		public final native String getVariable() /*-{ return this.variable; }-*/;

		public final String show() {
			if (isStatement()) {
				return getProposition();
			} else {
				return "ynq(" + getProposition() + ")";
			} // FIXME: handle other types of questions
		}
	}

	/* Common */

	public <T extends JavaScriptObject> JSONRequest sendRequest(String command, List<Arg> args, final JSONCallback<T> callback) {
		return JSONRequestBuilder.sendRequest(baseURL + "/" + command, args, callback);
	}

}