package se.chalmers.cs.mosg.client;

import se.chalmers.cs.gf.gwt.client.*;

import com.google.gwt.core.client.JavaScriptObject;

import java.util.List;
import java.util.ArrayList;

public class Semantics {

	private String baseURL;
	
	public Semantics (String baseURL) {
		this.baseURL = baseURL;
	}

	/* Interpretation */
	
	public JSONRequest interpret (String tree, final InterpretCallback callback) {
		List<Arg> args = new ArrayList<Arg>();
		args.add(new Arg("tree", tree));
		return sendRequest("interpret", args, callback);
	}

	public interface InterpretCallback extends JSONCallback<Interpretations> {  
	}

	public static class Interpretations extends JavaScriptObject {
		protected Interpretations() { }
		
		public final native IterableJsArray<Interpretation> getInterpretations() /*-{ return this.interpretations; }-*/;
	}

	public static class Interpretation extends JavaScriptObject {
		protected Interpretation() { }

		public final native boolean isStatement() /*-{ return this.type == "stm"; }-*/;
		public final native boolean isYesNoQuestion() /*-{ return this.type == "ynquest"; }-*/;
		public final native String getProp() /*-{ return this.prop; }-*/;
	}

	/* Common */

	public <T extends JavaScriptObject> JSONRequest sendRequest(String command, List<Arg> args, final JSONCallback<T> callback) {
		return JSONRequestBuilder.sendRequest(baseURL + "/" + command, args, callback);
	}

}