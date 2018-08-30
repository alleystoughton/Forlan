package jforlan.trees;

/** This file is a part of the Forlan toolset.  See the COPYING.txt
 *  file included with the source code.
 *
 *  Node is the basic unit of information in JTR.
 *  
 *  Jessica Sherrill
 *  3.3.2007
 */

import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;
import java.io.Serializable;
import java.util.Vector;
import jforlan.utils.ShapeUtil;

public class Node implements Serializable {

	private static final long serialVersionUID = 1L;
	
	// Variables //
	String label;
	private Vector<Node> children;
	private Node parent;
	private int x;
	private int y;

	// Constructors //
	public Node() {
		label = "";
		children = new Vector<Node>();
	}
	
	public Node(int x, int y) {
		label = "";
		this.x = x;
		this.y = y;
		children = new Vector<Node>();
	}

	public Node(String s) {
		label = s;
		children = new Vector<Node>();
	}
	
	public Node(String s, int x, int y) {
		label = s;
		this.x = x;
		this.y = y;
		children = new Vector<Node>();
	}

	// Functions //
	/*******************************************/
	/** Set methods **/
	/*******************************************/

	public void setLabel(String s) {
		label = s;
	}

	/*******************************************/
	/** Get methods **/
	/*******************************************/

	public String getLabel() {
		return label;
	}

	public Node getChild(int i) {
		return children.get(i);
	}

	public Vector<Node> getChildren() {
		return children;
	}

	/*******************************************/
	/** Add methods **/
	/*******************************************/

	//This method makes sure the child is added in the correct position relative to the other children (judged by x value)
	public void addChild(Node n) {
		n.setParent(this);
			
		boolean added = false;
		for (int i = 0; i < children.size(); i++) {
			if ((children.elementAt(i)).getX() > n.getX()) {
				children.add(i, n);
				added = true;
				break;
			}
		}
		if (!added) { 
			children.add(n);
		}
	}
	
	public boolean containsNodeRec(Node node) {
		if (children.contains(node)) 
			return true;
		else {
			for(int i = 0; i < children.size(); i++) {
				Node child = children.get(i);
				if (child.containsNodeRec(node))
					return true;
			}
		}

		return false;
	}

	/*******************************************/
	/** Remove methods **/
	/*******************************************/

	public void removeChild(Node n) {
		n.setParent(null);
		children.remove(n);
	}

	public void removeAllChildren() {
		children.clear();
	}

	public void setX(int pos_x) {
		this.x = pos_x;
	}

	public int getX() {
		return x;
	}

	public void setY(int pos_y) {
		this.y = pos_y;
	}

	public int getY() {
		return y;
	}
	
	public void setParent(Node parent) {
		this.parent = parent;
	}
	
	public Node getParent() {
		return parent;
	}
	
	//returns the proper ellipse to be drawn for this node
	public Ellipse2D.Float getEllipse(Graphics2D g2d) {
		return ShapeUtil.getEllipse(this, g2d);
	}
	
	//Moves this node and all of its children node "dist" distance
	public void moveXWithChildren(int dist) {
		this.setX(getX()+dist);
		for(int i = 0; i < this.getChildren().size(); i++) {
			(this.getChildren().get(i)).moveXWithChildren(dist);
		}
	}
	
	//returns the necessary width for this node combined with its children
	public int getNecessaryTreeDrawingWidth(Graphics2D g2d) {
		int childWidth = 0;
		for(int i = 0; i < this.getChildren().size(); i++) {
			Node node = this.getChildren().get(i);
			childWidth += node.getNecessaryTreeDrawingWidth(g2d);
		}
		
		return Math.max((int)this.getEllipse(g2d).getWidth() + 10, childWidth);
	}
}
