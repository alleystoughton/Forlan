package jforlan.automata;

/* Copyright (C) 2005 Leonard Lee

 The file is part of the Forlan toolset for experimenting with
 formal languages.  See the file COPYING.txt for copying and
 usage restrictions. */

import java.util.Vector;
import java.io.Serializable;

public class FA implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private Vector<State> states = new Vector<State>(); // all the states in the FA
	private Vector<Transition> transitions = new Vector<Transition>(); // all the transitions in the FA
	private State startState = null; // the state state

	// FA constructor
	public FA() {
	}

	public void setTransitions(Vector<Transition> transitions) {
		this.transitions = transitions;
	}

	public Vector<Transition> getTransitions() {
		return transitions;
	}

	public void setStates(Vector<State> states) {
		this.states = states;
	}

	public Vector<State> getStates() {
		return states;
	}

	public void setStartState(State startState) {
		this.startState = startState;
	}

	public State getStartState() {
		return startState;
	}
	
	public String toForlanString() {
		String s = "{states}";
        for (int i = 0; i < getStates().size(); i++)
        {
            if (i != 0) s += ",";
            s += getStates().elementAt(i).getLabel();
        }
        s += "{start state}";
        if (getStartState() != null) s += getStartState().getLabel();
        s += "{accepting states}";
        boolean first = true;
        for (int i = 0; i < getStates().size(); i++)
        {
            if (getStates().elementAt(i).isAccepting())
            {
                if (first) first = false;
                else s += ",";
                s += getStates().elementAt(i).getLabel();
            }
        }
        s += "{transitions}";
        Transition t;
        String transString = "";
        String transCurrent;
        // for each transition
        for (int i = 0; i < getTransitions().size(); i++)
        {
            t = getTransitions().elementAt(i);
            // for each label
            for (int j = 0; j < t.getLabels().size(); j++)
            {
                transCurrent = t.getSrc().getLabel() + "," + t.getLabels().elementAt(j) + "->";
                transCurrent += t.getDest().getLabel() + ";";
                transString += transCurrent;
            }
        }
        s += transString;
        // remove excess semicolon at the end
        if (s.lastIndexOf(';') != -1) 
        	s = s.substring(0,s.length() - 1);
        return s;
	}

}
