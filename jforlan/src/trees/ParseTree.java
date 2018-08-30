package jforlan.trees;

import jforlan.error.Error;
import jforlan.error.ErrorCollection;
import jforlan.forlan.ValidityCheck;
import jforlan.main.Controller;
import jforlan.main.JForlan;
import jforlan.main.MainFrame;
import jforlan.panels.TreeProject;
import jforlan.utils.Util;

/**
 * This file is a part of the Forlan toolset. See the COPYING.txt.txt file
 * included with the source code.
 * 
 * ParseTree is a representation of a parse tree used in Forlan.
 * 
 * Jessica Sherrill 3.3.2007
 */

public class ParseTree extends JTRTree {
	private static final long serialVersionUID = 1L;
	
	// Constructors //
	public ParseTree() {
		super();
	}

	// Functions //
	
	//returns the current tree formatted as a Forlan string
	public String toForlanString() {
		return recForlanFromParseTree(getRootI(0));
    }

    private String recForlanFromParseTree(Node n) {
        if (n.getChildren().size() == 0) {
        	return n.getLabel();
        }
        else {
        	String s = n.getLabel() + "("; 
            for (int i = 0; i < n.getChildren().size(); i++) {
            	s += recForlanFromParseTree(n.getChild(i));
                if (i+1 < n.getChildren().size()) s += ",";
            }
            s += ")";
            return s;
        }
    }
    
    public void updateErrors(TreeProject project) {
    	ErrorCollection errors  = project.getErrors();
    	errors.resetErrors();
		MainFrame mainFrame = JForlan.getMainFrame();

		if (getRoots().isEmpty()) {
			errors.addError(new Error(null, "A root node must exist."));
		}
		else if (getRoots().size() > 1) {
			errors.addError(new Error(null, "Too many root nodes."));
		}

		for(Node root : getRoots()) {
			updateErrorsRec(root, errors);
		}
		
		mainFrame.clearPrintArea();
		if (errors.isEmpty()) {
			String treeString = toForlanString();
			treeString = Controller.getForlanInterface().getPrettyParseTree(treeString);
			mainFrame.printLines(treeString);
		}
		else {
			errors.printErrors();
		}
    }
    
    //used by the updateErrors method
    private void updateErrorsRec(Node current, ErrorCollection errors) {
    	int numberOfChildren = current.getChildren().size();
    	String label = current.getLabel();

    	if (label.equals("%")) {
    		if (numberOfChildren != 0) {
    			errors.addError(new Error(current, "Cannot have children."));
    		}
    		if (current.getParent() == null) {
    			errors.addError(new Error(current, "Must have a parent."));
    		} else if (current.getParent().getChildren().size() != 1) {
    			errors.addError(new Error(current, "Must be an only child."));
    		}
    	} else {
    		ValidityCheck symbolCheck = Controller.getForlanInterface().checkSymbolValidity(label);
    		if (!symbolCheck.isValid()) {
    			if (label.equals(""))
					errors.addError(new Error(current, "Node missing label."));
				else if (Util.isDigitOrLetter(label.charAt(0)) || label.charAt(0) == '<')
					errors.addError(new Error(current, "Invalid symbol."));
				else
					errors.addError(new Error(current, "Invalid node label."));
    		}
    	}
		
    	for (Node child : current.getChildren()) {
			updateErrorsRec(child, errors);
		}
    }
}
