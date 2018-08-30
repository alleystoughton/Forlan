package jforlan.trees;

/** This file is a part of the Forlan toolset.  See the COPYING.txt
 *  file included with the source code.
 *
 *  JTRTree is an abstract class that allows easy use of projects
 *  with different kinds of trees.  It contains the required 
 *  functions of trees to be used by JTR.
 *  
 *  Jessica Sherrill
 *  4.25.2007
 *  
 *  Modified By Srinivasa Aditya Uppu
 *  5.12.2009
 */

import java.io.Serializable;
import java.util.Vector;

import javax.swing.JOptionPane;

import jforlan.main.JForlan;
import jforlan.panels.TreeProject;

public abstract class JTRTree implements Serializable {

	private static final long serialVersionUID = 1L;

	private Vector<Node> roots;

	JTRTree() {
		roots = new Vector<Node>();
	}

	public Node getRootI(int i) { //gets the i'th root from the collection of roots
		if (i >=0 && i < getRoots().size())
			return (Node) getRoots().elementAt(i);
		else
			return null;
	}

	public boolean isEmpty() {
		return getRoots().size() == 0;
	}

	public Vector<Node> getRoots() {
		return roots;
	}
	
	//must keep roots ordered, so we need a custom function for this
	private void addRoot(Node root) {
		root.setParent(null);
		int i = 0;
		while (roots.size() > i && root.getX() > roots.get(i).getX())
			i++;
		roots.add(i, root);
	}

	//second argument as null simply means to add it as a root. This method is used any time a node is added to the tree
	public void moveNode(Node child, Node parent) {
		if (parent == null) {
			removeNode(child);
			addRoot(child); //take into account ordering
		}
		else if (parent.equals(child)) {
			JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Cannot make a node its own parent", "Error", JOptionPane.ERROR_MESSAGE);
		}
		else {
			if (!child.containsNodeRec(parent)) {
				removeNode(child);
				parent.addChild(child);
			}
			else {
				JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Cannot create a loop in the tree!", "Error", JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	// node may not be in tree
	public void removeNode(Node node) {
		if (getRoots().contains(node)) {
			getRoots().remove(node);
			return;
		}
		else if (node.getParent() != null) {
			node.getParent().removeChild(node);
		}
	}

	public Vector<Node> getAllNodes() {
		Vector<Node> nodes = new Vector<Node>();
		
		for (int i = 0; i < roots.size(); i++)
			getAllNodesRec(roots.get(i), nodes);
		
		return nodes;
	}
	
	private void getAllNodesRec(Node node, Vector<Node> nodes) {
		nodes.add(node);
		for (int i = 0; i < node.getChildren().size(); i++) {
			getAllNodesRec(node.getChild(i), nodes);
		}
	}
	
	/*******************************************/
	/** Abstract methods **/
	/*******************************************/
			
	//returns the tree formatted as a Forlan string
	//only called when tree is valid
	public abstract String toForlanString();
	
	//updates the errors collection that is used for real-time error checking and displaying
	//displays the errors in the message text area
	//if there are no errors, displays pretty printed tree, instead
	public abstract void updateErrors(TreeProject project);

}
