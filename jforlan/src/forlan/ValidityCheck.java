package jforlan.forlan;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.util.Vector;

public class ValidityCheck {
	private boolean valid;
	private Vector<String> errors;
	private String formattedString; // or "error"
	
	public ValidityCheck(boolean valid, Vector<String> errors, String formattedString) {
		this.valid = valid;
		this.errors = errors;
		this.formattedString = formattedString;
	}

	public boolean isValid() {
		return valid;
	}

	public Vector<String> getErrors() {
		return errors;
	}

	public String getFormattedString() {
		return formattedString;
	}
	
	public void setFormattedString(String string) {
		formattedString = string;
	}
}
