package jforlan.panels;

/** This file is a part of the Forlan toolset.  See the COPYING.txt
 *  file included with the source code.
 *
 *  TreeProject is the graphical display of a tree used by a Project.
 *  It contains a reference to the tree in the project for painting
 *  purposes.
 *  
 *  Jessica Sherrill
 *  4.25.2007
 *  
 *  Modified By Srinivasa Aditya Uppu
 *  5.12.2009
 *  
 */
//package jforlan;

import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.io.Serializable;
import java.util.Vector;

import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;

import jforlan.automata.Arrow;
import jforlan.main.Controller;
import jforlan.trees.JTRTree;
import jforlan.trees.Node;
import jforlan.trees.ParseTree;
import jforlan.trees.ProgTree;
import jforlan.trees.RegTree;
import jforlan.utils.ShapeUtil;
import jforlan.utils.Util;

public class TreeProject extends Project implements Serializable, ActionListener {
	private static final long serialVersionUID = 1L;

	private JTRTree tree;
	
	private Point mouseLocation = new Point(0,0);

	private boolean settingParent = false; // in process of setting a node's parent

	// outside of handler, only one of these three will be non-null
	private Node clickedNode = null; 
	private Node clickedPopupNode = null;
	private Node toBeParentedNode = null; // node to be given a parent, non-null iff settingParent

    private JPopupMenu popupMenu;
    private JMenuItem editNodeMenuItem;
    private JMenuItem removeMenuItem;
	private JMenuItem changeParentMenuItem;
	
	// Constructors //
	public TreeProject(JTRTree t, String name, int type, boolean committable, Controller controller) {
		super(name, type, committable, controller);
		setTree(t);
		init();
	}
	
	public TreeProject(String name, int type, boolean committable, Controller controller) {
		super(name, type, committable, controller);
		if(type == Project.TYPE_PARSE_TREE)
			setTree(new ParseTree());
		else if(type == Project.TYPE_REG_TREE)
			setTree(new RegTree());
		else if(type == Project.TYPE_PROG_TREE)
			setTree(new ProgTree());
		
		init();
	}
	
	public void init() {
		this.setBackground(Color.white);
		
		this.addMouseListener(new TreeProjectMouseAdapter());
		this.addMouseMotionListener(new TreeProjectMouseMotionAdapter());
		
		popupMenu = new JPopupMenu();
		editNodeMenuItem = new JMenuItem("Edit Label");
		removeMenuItem = new JMenuItem("Remove Subtree");
		changeParentMenuItem = new JMenuItem("Change Parent");
		
		editNodeMenuItem.addActionListener(this);
		removeMenuItem.addActionListener(this);
		changeParentMenuItem.addActionListener(this);

		popupMenu.add(changeParentMenuItem);
		popupMenu.add(editNodeMenuItem);
		popupMenu.add(removeMenuItem);
		
		addComponentListener(new TreeProjectComponentAdapter());
	}

	public void paint(Graphics g) {
		super.paint(g);
		
		// init
		Graphics2D g2d = (Graphics2D) g;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		AffineTransform originalTransform = g2d.getTransform();

		////////DRAW THE TREE/////
		if (!getTree().isEmpty()) {
			for (int i = 0; i < getTree().getRoots().size(); i++) {
				drawRecursively(getTree().getRootI(i), 0, g2d);
			}
		
			///////DRAW "SET PARENT NODE" LINE////////
			if (settingParent && toBeParentedNode != null) {
				g2d.setColor(Color.red);
				g2d.drawLine(toBeParentedNode.getX(), toBeParentedNode.getY(), (int)mouseLocation.getX(), (int)mouseLocation.getY());
				
				Polygon arrow = new Polygon();
				double angle = Math.atan2((double) mouseLocation.getY() - (double) toBeParentedNode.getY(), (double) mouseLocation.getX() - (double) toBeParentedNode.getX());
				g2d.rotate(angle, mouseLocation.getX(), mouseLocation.getY());

				// arrowhead
				int arrowx = (int) mouseLocation.getX();
				int arrowy = (int) mouseLocation.getY();

				arrow.addPoint(arrowx, arrowy);
				arrow.addPoint(arrowx - 5, arrowy - 5);
				arrow.addPoint(arrowx - 5, arrowy + 5);
				g2d.fillPolygon(arrow);
				g2d.setTransform(originalTransform);
			}
		}
	}

	public void drawRecursively(Node parent, int depth, Graphics2D g2d) {
		FontMetrics fontMetrics = g2d.getFontMetrics();
		AffineTransform originalTransform = g2d.getTransform();
		
		///// draw lines connecting parents to children ////////
		g2d.setColor(Color.black);

		for(int i = 0; i < parent.getChildren().size(); i++) {
			Point2D nearestDstPoint = ShapeUtil.getNearestPointOnShape(parent.getChild(i).getX(), parent.getChild(i).getY(),  parent.getEllipse(g2d), null);
			
			g2d.drawLine(parent.getChild(i).getX(), parent.getChild(i).getY(), (int)nearestDstPoint.getX(), (int)nearestDstPoint.getY());
			
			//////////DRAW ARROW/////////////////////////

			Arrow arrow = ShapeUtil.getNodeArrow(parent.getChild(i), parent, g2d);
				
			if(arrow != null) {
				g2d.rotate(arrow.getAngle(), (int)arrow.getLocation().getX(), (int)arrow.getLocation().getY());
				
				g2d.fillPolygon(arrow.getPolygon());
				g2d.setTransform(originalTransform);
			}
		}
		
		////// DRAW NODE //////
		g2d.setColor(Color.white);
		g2d.fill(parent.getEllipse(g2d));
		
		if (!errors.getErrorsForObject(parent).isEmpty())
			g2d.setColor(Color.red);
		else
			g2d.setColor(Color.black);
		
		g2d.draw(parent.getEllipse(g2d));
		
		////// DRAW NODE LABEL ////////
		g2d.setColor(Color.blue);
		
		String label = parent.getLabel();
		int width = fontMetrics.stringWidth(label);
		int height = fontMetrics.getHeight();
		g2d.drawString(label, parent.getX() - (width/2), parent.getY() - 3 + height / 2);
		
		depth++;
		for(int i = 0; i < parent.getChildren().size(); i++) {
			drawRecursively(parent.getChild(i), depth, g2d);
		}
	}
	
	public Node getNodeAtLocation(int x, int y) {
		Graphics2D g2d = (Graphics2D) this.getGraphics();
		
		for(int i = 0; i < tree.getRoots().size(); i++) {
			Node nextRoot = tree.getRootI(i);
			if(nextRoot.getEllipse(g2d).contains(x,y)) {
				return nextRoot;
			}
			else {
				Node returnNode = getChildNodeAtLocationRec(nextRoot, x, y, g2d);
				if(returnNode != null)
					return returnNode;
			}
		}
		return null;
	}
	
	private Node getChildNodeAtLocationRec(Node parentNode, int x, int y, Graphics2D g2d) {
		for(int i = 0; i < parentNode.getChildren().size(); i++) {
			Node nextNode = parentNode.getChild(i);
			if(nextNode.getEllipse(g2d).contains(x,y)) {
				return nextNode;
			}
			else {
				Node returnNode = getChildNodeAtLocationRec(nextNode, x, y, g2d);
				if(returnNode != null)
					return returnNode;
			}
		}
		return null;
	}

	public void createNewNode(Point point) {
		Node node = new Node((int) point.getX(), (int) point.getY());
		renameNode(node);
		if(!node.getLabel().equals("")) {
			tree.moveNode(node, null);
		}
	}

	public void setTree(JTRTree t) {
		tree = t;
		updateErrors();
		performLayout();
		repaint();
	}

	public void performLayout() {
		if(tree.isEmpty())
			return;
	
		if(getWidth()==0)  //fixes bug where width of 0 is returned on a newly created TreeProject, the second call to performLayout handles it correctly
			return;

		Vector<Integer> widths = new Vector<Integer>();
		int totalWidth = 0;
		for(int i = 0; i < tree.getRoots().size(); i++) {
			Node root = tree.getRootI(i);
			int rootWidth = root.getNecessaryTreeDrawingWidth((Graphics2D)this.getGraphics());
			root.setX(rootWidth/2);
			widths.add(new Integer(rootWidth));
			totalWidth+=rootWidth;
			root.setY(30);
			performLayoutRec(root, rootWidth, 1);
		}
		
		int dx = 0;
		int startWidth = this.getWidth()/2 - totalWidth/2;
		
		for(int i = 0; i < tree.getRoots().size(); i++) {	
			Node root = (Node)tree.getRoots().get(i);
			int width = widths.get(i);
			root.moveXWithChildren(startWidth - root.getX() + dx + width/2);
			dx += width;
		}
		
		repaint();
	}
	
	private void performLayoutRec(Node node, int width, int depth) {
		int dx = 0;
		int startx;
		
		startx = node.getX() - width/2;
		
		for(int i = 0; i < node.getChildren().size(); i++) {
			Node child = (Node)node.getChildren().get(i);
			int childWidth = child.getNecessaryTreeDrawingWidth((Graphics2D)this.getGraphics());
			
			if(node.getChildren().size() == 1)
				child.setX(node.getX());
			else
				child.setX(startx + dx + childWidth/2);
			child.setY(30 + (60*depth));
			
			dx += childWidth;
			performLayoutRec(child, childWidth, depth+1);
		}
	}
	
	public JTRTree getTree() {
		return tree;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource().equals(editNodeMenuItem)) {
			renameNode(clickedPopupNode);
			clickedPopupNode = null;
		}
		else if (e.getSource().equals(removeMenuItem)) {
			tree.removeNode(clickedPopupNode);
			clickedPopupNode = null;
		}
		else if (e.getSource().equals(changeParentMenuItem)) {
			toBeParentedNode = clickedPopupNode;
			tree.moveNode(toBeParentedNode, null);
			settingParent = true;
			clickedPopupNode = null;
		}
		updateErrors();
		repaint();
	}
	
	@Override
	public void updateErrors() {
		tree.updateErrors(this);
	}
	
	public void renameNode(Node node) {
		if (node != null) {
			String input = (JOptionPane.showInputDialog(this, "Enter Label:", node.getLabel()));
			if (input != null) {
				input = Util.simplifyString(input);
				if (!input.equals("")) {
					node.setLabel(input);
				}
			}
		}
	}

	@Override
	public Rectangle calculateImageBounds() {
		int top = 0;
		int bottom = 0;
		int left = 0;
		int right = 0;
		
		Vector<Node> nodes = tree.getAllNodes();
		
		if(nodes.size() > 0) {
			right = nodes.get(0).getX() + (int)nodes.get(0).getEllipse((Graphics2D)this.getGraphics()).getWidth()/2;
			left = nodes.get(0).getX() - (int)nodes.get(0).getEllipse((Graphics2D)this.getGraphics()).getWidth()/2;
			top = nodes.get(0).getY() - (int)nodes.get(0).getEllipse((Graphics2D)this.getGraphics()).getHeight()/2;
			bottom = nodes.get(0).getY() + (int)nodes.get(0).getEllipse((Graphics2D)this.getGraphics()).getHeight()/2;
		}
		for(int i = 1; i < nodes.size(); i++) {
			int tempRight = nodes.get(i).getX() + (int)nodes.get(i).getEllipse((Graphics2D)this.getGraphics()).getWidth()/2;
			int tempLeft = nodes.get(i).getX() - (int)nodes.get(i).getEllipse((Graphics2D)this.getGraphics()).getWidth()/2;
			int tempTop = nodes.get(i).getY() - (int)nodes.get(i).getEllipse((Graphics2D)this.getGraphics()).getHeight()/2;
			int tempBottom = nodes.get(i).getY() + (int)nodes.get(i).getEllipse((Graphics2D)this.getGraphics()).getHeight()/2;
			
			if(tempTop < top)
				top = tempTop;
			if(tempBottom > bottom)
				bottom = tempBottom;
			if(tempLeft < left)
				left = tempLeft;
			if(tempRight > right)
				right = tempRight;
		}
		
		left -=2;
		right+=2;
		top-=2;
		bottom+=2;

		return new Rectangle(left, top, right-left, bottom - top);
	}
	
	public void resetMouseListeners() {
		for(MouseListener listener : this.getMouseListeners())
			this.removeMouseListener(listener);
		
		for(MouseMotionListener listener : this.getMouseMotionListeners())
			this.removeMouseMotionListener(listener);
		
		this.addMouseListener(new TreeProjectMouseAdapter());
		this.addMouseMotionListener(new TreeProjectMouseMotionAdapter());
	}
	
	private boolean isOptionClick(MouseEvent e) {
		return e.getButton() != 1 || e.isPopupTrigger() || (e.isControlDown() && e.getButton() == 1);
	}
	
	private class TreeProjectMouseAdapter extends MouseAdapter {
		@Override
		public void mousePressed(MouseEvent e) {
			clickedNode = getNodeAtLocation((int)e.getX(), (int)e.getY());
			
			if (clickedPopupNode != null) { // canceling popup
				clickedPopupNode = null;
				clickedNode = null;
			}
			else if (settingParent) { // setting parent
				if (clickedNode != null) {
					if (!clickedNode.getChildren().contains(toBeParentedNode))
						tree.moveNode(toBeParentedNode, clickedNode);
				}
				toBeParentedNode = null;
				settingParent = false;
				clickedNode = null;
			} else if (clickedNode == null) { // new node creation
				if (!isOptionClick(e)) {
					createNewNode(e.getPoint());
				}
			}
			else if (isOptionClick(e)) { // popup menu
				clickedPopupNode = clickedNode;
				clickedNode = null;
				editNodeMenuItem.setEnabled(true);
				changeParentMenuItem.setEnabled(true);
				removeMenuItem.setEnabled(true);
				popupMenu.show(e.getComponent(), (int) e.getPoint().getX(), (int) e.getPoint().getY());
			}
			// else beginning of node drag
			
			updateErrors();
			repaint();
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			if (clickedNode != null) { //done dragging node, location relative to siblings may have changed
				Node parentNode = clickedNode.getParent();
				tree.moveNode(clickedNode, parentNode);
				updateErrors();
			}
			clickedNode = null;
		}
	}
	
	private class  TreeProjectMouseMotionAdapter extends MouseMotionAdapter {
		@Override
		public void mouseDragged(MouseEvent e) {
			mouseLocation = e.getPoint();
			if (clickedNode != null) { // dragging node
				clickedNode.setX(e.getX());
				clickedNode.setY(e.getY());
				
				// keep states from having negative coordinates
				if (clickedNode.getX() - 25 < 0)
					clickedNode.setX(25);
				if (clickedNode.getY() - 25 < 0)
					clickedNode.setY(25);
				if(clickedNode.getX() + 25 > getWidth())
					clickedNode.setX(getWidth()-25);
				if(clickedNode.getY() + 25 > getHeight())
					clickedNode.setY(getHeight()-25);
			}
			repaint();
		}

		@Override
		public void mouseMoved(MouseEvent e) {
			mouseLocation = e.getPoint();
			if (settingParent) {
				repaint();
			}
		}
	}
	
	private class TreeProjectComponentAdapter extends ComponentAdapter {
		@Override
		public void componentResized(ComponentEvent e) {
			performLayout();
		}
	}
}
