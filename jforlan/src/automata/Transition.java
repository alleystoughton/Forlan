package jforlan.automata;

/* Copyright (C) 2005 Leonard Lee

 The file is part of the Forlan toolset for experimenting with
 formal languages.  See the file COPYING.txt for copying and
 usage restrictions.
 Modified By Srinivasa Aditya Uppu
 5.12.2009 */

import java.awt.Point;
import java.util.Vector;
import java.io.Serializable;

import jforlan.utils.ShapeUtil;

public class Transition implements Serializable {
	private static final long serialVersionUID = 1L;

	private State src = null; // the source State
	private State dest = null; // the destination State
	TransitionDot dot = null;
	private Vector<String> labels = new Vector<String>();
	private boolean customized = false;
	
	public Transition(State src, State dest) {
		this.src = src;
		this.dest = dest;

		standardizeTransitionDot();
	}
	
	public boolean isCustomized() {
		return customized;
	}
	
	public void standardizeTransitionDot() {
		if (this.isSelfPointing()) {
			dot = new TransitionDot(this, src.getX(), src.getY() - 50); //set the transition dot above the state
		}
		else { //set the dot between the two states
			Point midPoint = ShapeUtil.getMidPoint(src.getX(), src.getY(), dest.getX(), dest.getY());
			dot = new TransitionDot(this, (int)midPoint.getX(), (int)midPoint.getY());
		}
		setCustomized(false);
	}
	
	public void setCustomized(boolean customized) {
		this.customized = customized;
	}

	public void setSrc(State src) {
		this.src = src;
	}

	public State getSrc() {
		return src;
	}

	public void setDest(State dest) {
		this.dest = dest;
	}

	public State getDest() {
		return dest;
	}

	public void setLabels(Vector<String> labels) {
		this.labels = labels;
	}

	public Vector<String> getLabels() {
		return labels;
	}

	public TransitionDot getDot() {
		return dot;
	}
	
	public boolean isSelfPointing() {
		return src.equals(dest);
	}
}
