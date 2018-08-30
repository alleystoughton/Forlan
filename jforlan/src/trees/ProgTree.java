package jforlan.trees;

import java.util.Vector;

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
 * Srinivasa Aditya Uppu 1.27.2009
 */

public class ProgTree extends JTRTree {
	private static final long serialVersionUID = 1L;
	
	// Constructors //
	public ProgTree() {
		super();
	}
	
	//turns the current tree into a forlan-formatted string
	public String toForlanString() {
		return recForlanFromProgTree(getRootI(0));
    }
    
    public String recForlanFromProgTree(Node n) {
    	int num = n.getChildren().size();
    	
        if (num == 0) {
        	return n.getLabel();
        }
        else {
        	String s;
        	String label = n.getLabel();
        	if (label.endsWith(")")) {
        		s = label.replace(')', ',');
        	} else {
        		s = n.getLabel() + "(";
        	}
        	for (int i = 0; i < num; i++) {
        		s += recForlanFromProgTree(n.getChild(i));
        		if (i+1 < num) s += ",";
        	}
        	return s + ")";
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
			treeString = Controller.getForlanInterface().getPrettyProgTree(treeString);
			mainFrame.printLines(treeString);
		}
		else {
			errors.printErrors();
		}
    }
    
    // used by the following function
    private boolean badArgs(String s) {
    	if (s.contains("(")) {
    		return true;
    	}
    	else if (s.contains(")")) {
    		return true;
    	}
    	else {
    		return false;
    	}
    }
    
    private void updateErrorsRec(Node current, ErrorCollection errors) {
    	String label = current.getLabel();
    	int numChildren = current.getChildren().size();
    	int argIndex = label.indexOf('(');
    	String tag;
    	Vector<String> args = new Vector<String>();
    	if (argIndex == -1) {
    		tag = label;
    	}
    	else if (label.charAt(label.length()-1) != ')' || badArgs(label.substring(argIndex+1, label.length()-1))) {
    		errors.addError(new Error(current, "Invalid node label."));
			for(Node child : current.getChildren()) {
				updateErrorsRec(child, errors);
			}
			return; 		
    	}
    	else {
    		tag = label.substring(0, argIndex);
    		String argString = label.substring(argIndex+1, label.length()-1);
    		int start = 0;
    		int i = start;
    		int numBracket = 0;
    		while(i < argString.length()) {
    			if(argString.charAt(i) == '<')
    				numBracket++;
    			else if(argString.charAt(i) == '>')
    				numBracket--;
    			else if(argString.charAt(i) == ',') {
    				if(numBracket <= 0) {
    					if (start == i) {
    						args.add("");
    					}
    					else {
    						args.add(argString.substring(start, i));
    					}
    					start = i+1;
    					numBracket = 0;
    				}
    			}
    			i++;
    		}
    		
    		if (start == i) {
    			args.add("");
    		}
    		else {
    			args.add(argString.substring(start,i));
    		}
    		
    		// args.size() >= 1
    		for(String arg : args) {
       			if(arg.equals("")) {
    				errors.addError(new Error(current, "Invalid node label."));
    				for(Node child : current.getChildren()) {
    					updateErrorsRec(child, errors);
    				}
    				return;
    			}
    		}
     	}
			
    	if (tag.equals("var")) {
    		if(args.size() != 1) {
    			errors.addError(new Error(current,"Needs 1 variable."));
    		}
    		else if (!Util.isValidProgVariable(args.get(0))) {
    			errors.addError(new Error(current,args.get(0) + " is not a variable."));
    		}
    		else if (numChildren != 0) {
    			errors.addError(new Error(current,"Should have 0 children."));
    		}
    	}
    	else if (tag.equals("const")) {
    		if(args.size() != 1) {
    			errors.addError(new Error(current,"Needs 1 constant."));
    		}
    		else if (!Util.isValidProgConst(args.get(0))) {
    			errors.addError(new Error(current,args.get(0) + " is not a constant."));
    		}
    		else if (numChildren != 0) {
    			errors.addError(new Error(current,"Should have 0 children."));
    		}
    	}
    	else if (tag.equals("int")) {
    		if(args.size() != 1) {
    			errors.addError(new Error(current,"Needs 1 numeral."));
    		}
    		else if (!Util.isValidProgNumeral(args.get(0))) {
    			errors.addError(new Error(current, args.get(0) + " is not a numeral."));
    		}
    		else if (numChildren != 0) {
    			errors.addError(new Error(current,"Should have 0 children."));
    		}
    	}
    	else if (tag.equals("sym")) {
    		if(args.size() != 1) {
    			errors.addError(new Error(current,"Needs 1 symbol."));
    		}
    		else {
    			ValidityCheck check = Controller.getForlanInterface().checkSymbolValidity(args.get(0));
    			if (!check.isValid())
    			{
    				errors.addError(new Error(current,args.get(0) + " is not a symbol."));
    			}
    			else if (numChildren != 0) {
    				errors.addError(new Error(current,"Should have 0 children."));
    			}
    		}
    	}
    	else if (tag.equals("str")) {
    		if(args.size() != 1) {
    			errors.addError(new Error(current,"Needs 1 string."));
    		}
    		else {
    			ValidityCheck check = Controller.getForlanInterface().checkStringValidity(args.get(0));
    			if (!check.isValid())
    			{
    				errors.addError(new Error(current,args.get(0) + " is not a string."));
    			}
    			else if (numChildren != 0) {
    				errors.addError(new Error(current,"Should have 0 children."));
    			}
    		}
    	}
    	else if (tag.equals("calc")) {
    		if(args.size() != 1) {
    			errors.addError(new Error(current,"Needs 1 operator."));
    		}
    		else if (!Util.isValidProgOperator(args.get(0))) {
    			errors.addError(new Error(current,args.get(0) + " is not an operator."));
    		}
    		else if (numChildren != 1)
    			errors.addError(new Error(current,"Needs 1 child."));
    	}
    	else if (tag.equals("pair")) {
    		if(args.size() != 0) {
    			errors.addError(new Error(current,"Should have 0 arguments."));
    		}
    		else if (numChildren != 2) {
    			errors.addError(new Error(current,"Needs 2 children."));
    		}
    	}
    	else if (tag.equals("app")) {
    		if(args.size() != 0) {
    			errors.addError(new Error(current,"Should have 0 arguments."));
    		}
    		else if(numChildren != 2)
    			errors.addError(new Error(current,"Needs 2 children."));
    		
    	}
    	else if (tag.equals("cond")) {
    		if(args.size() != 0) {
    			errors.addError(new Error(current,"Should have 0 arguments."));
    		}
    		else if(numChildren != 3) {
    			errors.addError(new Error(current,"Needs 3 children."));
    		}
			}
    	else if (tag.equals("lam")) {
    		if(args.size() != 1) {
    			errors.addError(new Error(current,"Needs 1 variable."));
    		}
    		else if (!Util.isValidProgVariable(args.get(0))) {
    			errors.addError(new Error(current,args.get(0) + " is not a variable."));
    		}
    		else if (numChildren != 1) {
    			errors.addError(new Error(current,"Needs 1 child."));
    		}
    	}
    	else if (tag.equals("letSimp")) {
    		if(args.size() != 1) {
    			errors.addError(new Error(current,"Needs 1 variable."));
    		}
    		else if (!Util.isValidProgVariable(args.get(0))) {
    			errors.addError(new Error(current,args.get(0) + "is not a variable."));
    		}
    		else if (numChildren != 2) {
    			errors.addError(new Error(current,"Needs 2 children."));
    		}
    	}
    	else if (tag.equals("letRec")) {
    		if(args.size() != 2) {
    			errors.addError(new Error(current,"Needs 2 variables."));
    		}
    		else {
    			boolean errorFound = false;
    			if (!Util.isValidProgVariable(args.get(0))) {
    				errors.addError(new Error(current,args.get(0) + " is not a variable."));
    				errorFound = true;
    			}
    			if (!Util.isValidProgVariable(args.get(1))) {
    				errors.addError(new Error(current,args.get(1) + " is not a variable."));
    				errorFound = true;
    			}
    			if(numChildren != 2 && !errorFound)
    				errors.addError(new Error(current,"Needs 2 children."));
    		}
    	}
    	else {
    		errors.addError(new Error(current,"Invalid node label."));
    	}
		
		for (Node child : current.getChildren()) {
			updateErrorsRec(child, errors);
		}
    }
}
