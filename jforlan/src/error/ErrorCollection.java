package jforlan.error;

/* The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. */

import java.io.Serializable;
import java.util.Vector;

import jforlan.main.JForlan;
import jforlan.main.MainFrame;
import jforlan.trees.Node;
import jforlan.automata.State;
import jforlan.automata.Transition;

public class ErrorCollection implements Serializable  {
	
	private static final long serialVersionUID = 1L;

	private Vector<Error> errors;
	
	public ErrorCollection() {
		errors = new Vector<Error>();
	}
	
	public void addError(Error error) {
		errors.add(error);
	}
	
	public Vector<Error> getTreeErrors() {
		return errors;
	}
	
	public Vector<String> getErrorsForObject(Object object) {
		Vector<String> objectErrors = new Vector<String>();
		for (Error error : errors) {
			if (error.getObject() != null && error.getObject().equals(object)) {
				objectErrors.add(error.getError());
			}
		}
		return objectErrors;
	}
	
	public void resetErrors() {
		errors.removeAllElements();
	}
	
	public boolean isEmpty() {
		return errors.isEmpty();
	}
	
	public int size() {
		return errors.size();
	}
	
	public Error get(int i) {
		return errors.get(i);
	}
	
	public void printErrors() {
		MainFrame mainFrame = JForlan.getMainFrame();
		mainFrame.clearPrintArea();
		if (errors.size() == 0) {
			mainFrame.printLine("No Errors Found!");
		}
		else if (errors.size() == 1) {
			mainFrame.printLine("1 Error Found!");
		}
		else {
			mainFrame.printLine(errors.size() + " Errors found!");
		}
		
		for (Error error : errors) {
			if (error.getObject() == null) {
				mainFrame.printLine("      " + error.getError());
			}
			else if (error.getObject() instanceof State) {
				mainFrame.printLine("      " + ((State) error.getObject()).getLabel() + ": " + error.getError());
			}
			else if (error.getObject() instanceof Transition) {
				String labelString = ((Transition) error.getObject()).getLabels().toString();
				labelString = labelString.substring(1, labelString.length()-1);
				mainFrame.printLine("      " + labelString + ": " + error.getError());
			}
			else if (error.getObject() instanceof Node) {
				mainFrame.printLine("      " + ((Node) error.getObject()).getLabel() + ": " + error.getError());
			}
		}
	}
}
