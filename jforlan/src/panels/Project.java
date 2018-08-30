package jforlan.panels;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.awt.Rectangle;
import javax.swing.JPanel;
import jforlan.error.ErrorCollection;
import jforlan.main.Controller;

public abstract class Project extends JPanel {
	private static final long serialVersionUID = 1L;
	
	public static int TYPE_FA = 1;
	public static int TYPE_RFA = 2;
	
	public static int TYPE_PARSE_TREE = 3;
	public static int TYPE_REG_TREE = 4;
	public static int TYPE_PROG_TREE = 5;
	
	int type;
	String name;
	boolean committable;

	ErrorCollection errors;
	
	transient Controller controller;
	
	public Project(String name, int type, boolean committable, Controller controller) {
		this.name = name;
		this.type = type;
		this.committable = committable;
		this.controller = controller;
		errors = new ErrorCollection();
	}

	public abstract Rectangle calculateImageBounds();

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public boolean isCommittable() {
		return committable;
	}
	
	public void setCommittable(boolean b) {
		committable = b;
	}
	
	public abstract void updateErrors();

	public ErrorCollection getErrors() {
		return errors;
	}
}
