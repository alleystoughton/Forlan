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
 * RegTree extends JTRTree and is a representation for a tree of Regular
 * Expressions.
 * 
 * Jessica Sherrill 3.3.2007
 */

public class RegTree extends JTRTree {
	private static final long serialVersionUID = 1L;

	// Constructors //
	public RegTree() {
		super();
	}

	// Functions //
	
	//turns the current tree into a Forlan-formatted string
	public String toForlanString() {
		return recForlanFromRegTree(getRootI(0));
	}
    
    private String recForlanFromRegTree(Node n){
    	int num = n.getChildren().size();
        
        if(num == 0) {
        	return n.getLabel();
        }
        else {
        	String s = "(" + recForlanFromRegTree(n.getChild(0)) + ")";
        
        	if (!n.getLabel().equals("@")) s += n.getLabel();
        
        	// there may not be a right child
          
        	if (n.getChildren().size() == 2) {
        		s += "(" + recForlanFromRegTree(n.getChild(1)) + ")";
        	}
        	return s;
        }
    }
    
    //updates the errors vector for this tree. This is used for real-time error checking and displaying
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
			treeString = Controller.getForlanInterface().getPrettyRegTree(treeString);
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

		if (label.equals("%") || label.equals("$")) {
			if (numberOfChildren != 0) {
				errors.addError(new Error(current, "Should have 0 children."));
			}
		} else if (label.equals("*")) {
			if (numberOfChildren != 1) {
				errors.addError(new Error(current, "Should have 1 child."));
			}	
		} else if (label.equals("@") || label.equals("+")) {
			if (numberOfChildren != 2) {
				errors.addError(new Error(current, "Should have 2 children."));
			}	
		} else {
    		ValidityCheck symbolCheck = Controller.getForlanInterface().checkSymbolValidity(label);
    		if (symbolCheck.isValid()) {
    			if (numberOfChildren != 0) {
    				errors.addError(new Error(current, "Should have 0 children."));
    			}  			
    		}
    		else {
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
