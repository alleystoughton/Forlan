package jforlan.automata;

/* Copyright (C) 2005 Leonard Lee

 The file is part of the Forlan toolset for experimenting with
 formal languages.  See the file COPYING.txt for copying and
 usage restrictions. */

import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;
import java.io.Serializable;

import jforlan.utils.ShapeUtil;

public class State implements Serializable {
	private static final long serialVersionUID = 1L;

	private int x;
	private int y;
	
	private String label = ""; // label is used for file output & tab label
	private boolean accepting = false; // accepting state?

	// State constructor
	public State(int xcoord, int ycoord) {
		setX(xcoord);
		setY(ycoord);
	}
	
	public State(String label, int xcoord, int ycoord) {
		this.label = label;
		setX(xcoord);
		setY(ycoord);
	}

	public void setLocation(int x1, int y1) {
		setX(x1);
		setY(y1);
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public String getLabel() {
		return label;
	}

	public void setAccepting(boolean accepting) {
		this.accepting = accepting;
	}

	public boolean isAccepting() {
		return accepting;
	}

	public void setX(int x) {
		this.x = x;
	}
	
	public int getX() {
		return x;
	}

	public void setY(int y) {
		this.y = y;
	}

	public int getY() {
		return y;
	}
	
	public Ellipse2D.Float getEllipse(Graphics2D g2d) {
		return ShapeUtil.getEllipse(this, g2d);
	}
}
