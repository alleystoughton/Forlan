package jforlan.error;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.io.Serializable;

public class Error implements Serializable {
	private static final long serialVersionUID = 1L;

	private Object object;
	private String error;
	
	public Error(Object object, String error) {
		this.object = object;
		this.error = error;
	}

	public Object getObject() {
		return object;
	}

	public void setObject(Object object) {
		this.object = object;
	}

	public String getError() {
		return error;
	}

	public void setError(String error) {
		this.error = error;
	}	
}
