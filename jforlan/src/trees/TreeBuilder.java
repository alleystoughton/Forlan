package jforlan.trees;

/** This file is a part of the Forlan toolset.  See the COPYING.txt
 *  file included with the source code.
 *
 *  JTRUtil extends the jfa.Util class for use of the symbol parsing
 *  function.  JTRUtil handles the translation of ParseTrees and
 *  RegTrees to and from Forlan.
 *
 *  Jessica Sherrill
 *  5.20.2007
 *  Modified By Srinivasa Aditya Uppu
 *  5.12.2009
 */

import jforlan.utils.Util;

public class TreeBuilder extends Util {

    // Constructors //
    public TreeBuilder() {
    }
    
    // Functions // 
    // these tree building functions are only called on valid Forlan trees
    
    public static ParseTree buildParseTreeFromForlan(String s) {
    	ParseTree tree = new ParseTree();
    	Node rootNode;
    	
    	if (s.contains("(")) {
    		String label = s.substring(0, s.indexOf('('));
    		rootNode = new Node(label);
    		recTreeFromForlan(rootNode, s.substring(s.indexOf('(')));
    	}
    	else if (!s.isEmpty())
    		rootNode = new Node(s);
    	else
    		return tree;
    	
    	tree.moveNode(rootNode, null);
    	return tree;
    }
    
    // used for building all three kinds of trees
    public static void recTreeFromForlan(Node current, String childrenString) {
    	int numParen = 1; // how deeply are we nested at position i?
    	int i = 1;
    	int start = 1; // start of next child
    	while (true) {
    		if (numParen == 1 && ((childrenString.charAt(i) == ',') || (childrenString.charAt(i) == ')'))) {
    			int parenIndex = childrenString.indexOf('(', start);
    			if ((parenIndex > 1) && (parenIndex < i)) {
    				Node node = new Node(childrenString.substring(start, parenIndex));
    				recTreeFromForlan(node, childrenString.substring(parenIndex, i));
    				current.addChild(node);
    				node.setParent(current);
    			}
    			else {
    				Node node = new Node(childrenString.substring(start, i));
    				current.addChild(node);
    				node.setParent(current);
    			}
    			if (childrenString.charAt(i) == ')')
    				break;
    			start = i+1;
    		}
       		else if (childrenString.charAt(i) == '(')
    			numParen++;
    		else if (childrenString.charAt(i) == ')') {
    			numParen--;
    		}
    		i++;
    	}
    }
    
    // builds a regular expression tree from the fully parenthesized form
    // provided by Forlan
    public static RegTree buildRegTreeFromForlan(String s) {
    	RegTree tree = new RegTree();
    	Node rootNode;
    	
    	if (s.contains("(")) {
    		String label = s.substring(0, s.indexOf('('));
    		rootNode = new Node(label);
    		recTreeFromForlan(rootNode, s.substring(s.indexOf('(')));
    	}
    	else if (!s.isEmpty())
    		rootNode = new Node(s);
    	else
    		return tree;
    	
    	tree.moveNode(rootNode, null);
    	return tree;
    }
    
    private static String[] tags = {"var", "const", "int", "sym", "str", "pair", "calc", "app", "cond", "lam", "letSimp", "letRec"};
    private static int[] tagsChildren = {0, 0, 0, 0, 0, 2, 1, 2, 3, 1, 2, 2};
    
    private static int findNumChildrenOfTag(String tag) {
    	for (int i = 0; i < tags.length; i++) {
    		if (tag.equals(tags[i])) {
    			return tagsChildren[i];
    		}
    	}
    	return -1;
    }

    // builds a program tree from Forlan. Initially, it loads the tree like a parse tree, then calls "compressProgTree" to properly format it
    public static ProgTree buildProgTreeFromForlan(String s) {
    	ProgTree tree = new ProgTree();
    	Node rootNode;
    	
    	if (s.contains("(")) {
    		String label = s.substring(0, s.indexOf('('));
    		rootNode = new Node(label);
    		recTreeFromForlan(rootNode, s.substring(s.indexOf('(')));
    		compressProgTree(rootNode);
    	}
    	else if (!s.isEmpty())
    		rootNode = new Node(s);
    	else
    		return tree;
    	
    	tree.moveNode(rootNode, null);
    	return tree;
    }
    
    //properly formats a program tree loaded as a parse tree by pushing certain nodes up to the parent label
    //to be arguments of tags
    private static void compressProgTree(Node node) {
    	for (Node child : node.getChildren()) {
    		compressProgTree(child);
    	}
    	String label = node.getLabel();
    	int numChildren = findNumChildrenOfTag(label);
    	if (numChildren >= 0 && numChildren < node.getChildren().size()) {
    		label += "(";
    		while (node.getChildren().size() > numChildren) {
				Node firstNode = node.getChildren().get(0);
				label += firstNode.getLabel();
				node.removeChild(firstNode);
				if (node.getChildren().size() > numChildren)
					label += ",";
			}
			label += ")";
			node.setLabel(label);    		
    	}
     }
}
