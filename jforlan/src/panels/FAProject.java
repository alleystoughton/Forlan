package jforlan.panels;

/* Copyright (C) 2005 Leonard Lee

 The file is part of the Forlan toolset for experimenting with
 formal languages.  See the file COPYING.txt for copying and
 usage restrictions. */
// Modified By Srinivasa Aditya Uppu
//  5.12.2009

import java.awt.*;

import javax.swing.*;

import java.util.Vector;
import java.awt.geom.*;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;

import java.io.Serializable;

import jforlan.automata.Arrow;
import jforlan.automata.FA;
import jforlan.automata.State;
import jforlan.automata.Transition;
import jforlan.automata.TransitionDot;
import jforlan.dialogs.EditLabelsDialog;
import jforlan.error.Error;
import jforlan.forlan.ValidityCheck;
import jforlan.main.Controller;
import jforlan.main.JForlan;
import jforlan.main.MainFrame;
import jforlan.utils.ShapeUtil;
import jforlan.utils.Util;

public class FAProject extends Project implements Serializable, ActionListener  {
	private static final long serialVersionUID = 1L;

	private State currentDraggedState;
	private State srcStateClicked; // source of new transition
	private Point destPoint; // current destination of new transition
	private TransitionDot currentDraggedTransitionDot;
	
	private State popupState;
	private TransitionDot popupTransDot;
	
	private JPopupMenu statePopupMenu;
	private JMenuItem startStateMenuItem;
	private JMenuItem editStateMenuItem;
	private JCheckBoxMenuItem acceptingStateMenuItem;
	private JMenuItem removeMenuItem;
	
	private JPopupMenu transitionPopupMenu;
	private JMenuItem editTransMenuItem;
	private JMenuItem reverseMenuItem;
	private JMenuItem viewTreeMenuItem;
	private JMenuItem standardizeMenuItem;
	private JMenuItem removeTransMenuItem;
	
	private FA fa;

	private Vector<Rectangle> labelBounds; //sort of hack to get around non-persistent label placement

	public FAProject(String name, int type, boolean committable, Controller controller) {
		super(name, type, committable, controller);
		fa = new FA();

		updateErrors();
		
		try {
			init();
		}
		catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	public FAProject(FA fa, String name, int type, boolean committable, Controller controller) {
		super(name, type, committable, controller);
		this.fa = fa;
		
		updateErrors();
		
		try {
			init();
		}
		catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	private void init() throws Exception {
		this.setBackground(Color.WHITE);
		this.addMouseMotionListener(new FAPanelMouseMotionAdapter());
		this.addMouseListener(new FAPanelMouseAdapter(this));

		labelBounds = new Vector<Rectangle>();
		
		destPoint = new Point(0,0);
		
		statePopupMenu = new JPopupMenu();
		startStateMenuItem = new JMenuItem();
		editStateMenuItem = new JMenuItem();
		acceptingStateMenuItem = new JCheckBoxMenuItem();
		removeMenuItem = new JMenuItem();
		
		transitionPopupMenu = new JPopupMenu();
		editTransMenuItem = new JMenuItem();
		reverseMenuItem = new JMenuItem();
		viewTreeMenuItem = new JMenuItem();
		standardizeMenuItem = new JMenuItem();
		removeTransMenuItem = new JMenuItem();
		
		startStateMenuItem.setText("Make Start State");
		startStateMenuItem.addActionListener(this);
		editStateMenuItem.setText("Edit Label");
		editStateMenuItem.addActionListener(this);
		acceptingStateMenuItem.setText("Accepting State");
		acceptingStateMenuItem.addActionListener(this);
		removeMenuItem.setText("Remove State");
		removeMenuItem.addActionListener(this);

		viewTreeMenuItem.setText("View Label as Tree");
		viewTreeMenuItem.addActionListener(this);
		standardizeMenuItem.setText("Standardize Transition");
		standardizeMenuItem.addActionListener(this);

		if(this.type == TYPE_FA)
			editTransMenuItem.setText("Edit Labels");
		else
			editTransMenuItem.setText("Edit Label");
		editTransMenuItem.addActionListener(this);
		reverseMenuItem.setText("Reverse Transition");
		reverseMenuItem.addActionListener(this);
		removeTransMenuItem.setText("Remove Transition");
		removeTransMenuItem.addActionListener(this);
		
		statePopupMenu.add(acceptingStateMenuItem);
		statePopupMenu.add(startStateMenuItem);
		statePopupMenu.add(editStateMenuItem);
		statePopupMenu.add(removeMenuItem);
		
		transitionPopupMenu.add(editTransMenuItem);
		transitionPopupMenu.add(reverseMenuItem);
		transitionPopupMenu.add(removeTransMenuItem);
		transitionPopupMenu.add(viewTreeMenuItem);
		transitionPopupMenu.add(standardizeMenuItem);
	}

	public void paint(Graphics g) {
		super.paint(g);
		
		Vector<QuadCurve2D.Float> drawnTransitions = new Vector<QuadCurve2D.Float>();
		Vector<Shape> drawnSelfTransitions = new Vector<Shape>();
		Vector<Rectangle> drawnLabels = new Vector<Rectangle>();
		
		// init
		Graphics2D g2d = (Graphics2D) g;
		AffineTransform originalTransform = g2d.getTransform();
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		FontMetrics fontMetrics = g2d.getFontMetrics();
		
		//////////////////DRAW TRANSITIONS/////////////////
		
		Vector<Transition> transitions = fa.getTransitions();
		
		for(int i = 0; i < transitions.size(); i++) {
			Transition nextTransition = transitions.elementAt(i);
			TransitionDot nextDot = nextTransition.getDot();
			
			if(!errors.getErrorsForObject(nextTransition).isEmpty())
				g2d.setColor(Color.red);
			else
				g2d.setColor(Color.black);
			
			if(nextTransition.isSelfPointing()) {
				Ellipse2D.Float nonRotatedTransitionLine = ShapeUtil.getNonRotatedSelfTransitionLine(nextTransition, g2d);
				double angle = ShapeUtil.getAngleBetweenPoints(new Point(nextTransition.getSrc().getX(),nextTransition.getSrc().getY()) , new Point(nextDot.getX(), nextDot.getY()));
				
				AffineTransform rotateTransform = new AffineTransform();
				rotateTransform.rotate(angle,nextTransition.getSrc().getX(),nextTransition.getSrc().getY());
				
				Shape transitionLine = rotateTransform.createTransformedShape(nonRotatedTransitionLine);
				
				drawnSelfTransitions.add(transitionLine);
				g2d.draw(transitionLine);
				
				//////////DRAW ARROW/////////////////////////
				
				Arrow arrow = ShapeUtil.getSelfTransitionArrow(nextTransition, transitionLine, g2d);
				if(arrow != null) {
					g2d.rotate(arrow.getAngle(), (int)arrow.getLocation().getX(), (int)arrow.getLocation().getY());
					g2d.fillPolygon(arrow.getPolygon());
					g2d.setTransform(originalTransform);
				}
			}
			else  {
				//test whether or not it is bidirectional
				boolean bidirectional = false;
				boolean secondDrawn = false;

				if(!nextTransition.isCustomized()) { //test for bidirectional
					for(Transition tempTransition : transitions) {	
						if(nextTransition.equals(tempTransition)) {
							continue;
						}
						else if(tempTransition.getDest().equals(nextTransition.getSrc()) && tempTransition.getSrc().equals(nextTransition.getDest()) 
								&& !tempTransition.isSelfPointing() && !nextTransition.isSelfPointing()) {
							bidirectional = true;
				
							//see if the other one has been drawn yet
							for(Transition testTransition : transitions) {
								if(testTransition.equals(tempTransition)) {
									secondDrawn = true;
								}
							}
							break;
						}
					}
				}
				
				QuadCurve2D.Float transitionLine = ShapeUtil.getTransitionLine(nextTransition, g2d, bidirectional, secondDrawn);
				drawnTransitions.add(transitionLine);
				g2d.draw(transitionLine);
				
				//////////DRAW ARROW/////////////////////////
				
				Arrow arrow = ShapeUtil.getTransitionArrow(nextTransition, bidirectional,transitionLine, g2d);
				if(arrow != null) {
					g2d.rotate(arrow.getAngle(), (int)arrow.getLocation().getX(), (int)arrow.getLocation().getY());
						
					g2d.fillPolygon(arrow.getPolygon());
					g2d.setTransform(originalTransform);
				}
			}
			
			//////////DRAW TRANSITION DOT///////////////////////
			Ellipse2D.Float transitionDotEllipse = nextTransition.getDot().getEllipse();
			g2d.fill(transitionDotEllipse);
		}
		
		//////////DRAW TRANSITION LABELS//////////////////////   (must be done after all transitions so we know where the curves ended up)
		g2d.setColor(Color.magenta);
		labelBounds.removeAllElements();
		
		for(Transition nextTransition : transitions) {
			TransitionDot nextDot = nextTransition.getDot();
			Ellipse2D.Float transitionDotEllipse = nextDot.getEllipse();
			
			String label = nextTransition.getLabels().toString().substring(1, nextTransition.getLabels().toString().length()-1);
			int width = fontMetrics.stringWidth(label);
			int height = fontMetrics.getHeight();
			
			double x = transitionDotEllipse.getX();
			double y = transitionDotEllipse.getY()-4;
			
			boolean locationFound = false;
			double constant = .8;
			double counter = 3;
			double counter_inc = .4;
			double r = 0;
			double dx = 0;
			double dy = 0;
			Rectangle textRect = null;
			
			while(!locationFound) {
				boolean intersectsTransition = false;
				boolean intersectsState = false;
				boolean intersectsLabel = false;

				r = constant*counter;
				dx = r*Math.cos(counter)- width/2; //move left for half the label's size
				dy = r*Math.sin(counter)-5; //favor top over bottom
				
				textRect = new Rectangle(new Point((int)x + (int)dx -10, (int)y + (int)dy - 10), new Dimension(width+20, height+20));
				//Ellipse2D.Float ellipse = new Ellipse2D.Float((float)x + (float)dx, (float)y+(float)dy, 1, 1);
				//g2d.draw(ellipse);
				//g2d.draw(textRect);
				for(int j = 0; j < drawnTransitions.size(); j++) {
		
					if(ShapeUtil.isRectNearCurve(drawnTransitions.get(j), textRect, g2d)) {
						intersectsTransition = true;
						break;
					}
				}
				
				for(int j = 0; j < drawnSelfTransitions.size(); j++) {
					if(drawnSelfTransitions.get(j).intersects(textRect)) {
						intersectsTransition = true;
						break;
					}
				}
				
				for(int j = 0; j < fa.getStates().size(); j++) {
					if(textRect.intersects(((State)fa.getStates().get(j)).getEllipse(g2d).getBounds())) {
						intersectsState = true;
						break;
					}	
				}
				
				for(int j = 0; j < drawnLabels.size(); j++) {
					if(textRect.intersects(drawnLabels.get(j))) {
						intersectsLabel = true;
						break;
					}	
				}
				
				if(!intersectsTransition && !intersectsState && !intersectsLabel) {
					locationFound = true;
				}
				
				counter = counter + counter_inc;
			}
			//g2d.draw(textRect);
			g2d.drawString(label,(int) (x + dx),(int) (y+dy + height));
			drawnLabels.add(textRect);
			labelBounds.add(textRect);
		}
		g2d.setColor(Color.BLACK);
		
		////////////////// DRAW STATES //////////////////////////

		for (State tempState : fa.getStates()) {
			Ellipse2D.Float stateEllipse = tempState.getEllipse(g2d);
			
			g2d.setColor(Color.white);
			g2d.fill(stateEllipse);
			
			if(!errors.getErrorsForObject(tempState).isEmpty())
				g2d.setColor(Color.red);
			else
				g2d.setColor(Color.black);
			
			g2d.draw(stateEllipse);
			
			// DRAW ACCEPTING SATE IF NECESSARY
			if (tempState.isAccepting()) 
				g2d.draw(ShapeUtil.getAcceptingEllipse(tempState,g2d));
			
			// DRAW LABEL
			g2d.setColor(Color.blue);
			String label = tempState.getLabel();
			int width;
			if(label != null)
				width = fontMetrics.stringWidth(label);
			else
				width = 0;
			int height = fontMetrics.getHeight();
			g2d.drawString(label, tempState.getX() - (width/2), tempState.getY() - 3 + height / 2);
			
			// DRAW START STATE INDICATOR
			if (tempState.equals(fa.getStartState())) {
				g2d.setColor(Color.black);
				width = fontMetrics.stringWidth("Start");
				Ellipse2D.Float startEllipse = tempState.getEllipse(g2d);
				g2d.drawString("Start", tempState.getX() - width - startEllipse.width/2 -20, tempState.getY() - 3 + height / 2);
				
				// arrow line
				g2d.drawLine(tempState.getX() - (int)startEllipse.width/2 - 20, tempState.getY(), tempState.getX() - (int)startEllipse.width/2, tempState.getY());
				
				// arrowhead
				int arrowx = tempState.getX() - (int)startEllipse.width/2;
				int arrowy = tempState.getY();
				
				Polygon arrow = new Polygon();
				arrow.addPoint(arrowx, arrowy);
				arrow.addPoint(arrowx - 5, arrowy - 5);
				arrow.addPoint(arrowx - 5, arrowy + 5);
				g2d.fillPolygon(arrow);
			}
		}
		
		//DRAW TRANSITION-IN-PROGRESS (red)
		if (srcStateClicked != null) {
			g2d.setColor(Color.red);
			g2d.drawLine(srcStateClicked.getX(), srcStateClicked.getY(), destPoint.x, destPoint.y);

			Polygon arrow = new Polygon();
			double angle = Math.atan2((double) destPoint.y - (double) srcStateClicked.getY(), (double) destPoint.x - (double) srcStateClicked.getX());
			g2d.rotate(angle, destPoint.x, destPoint.y);

			// arrowhead
			int arrowx = destPoint.x;
			int arrowy = destPoint.y;

			arrow.addPoint(arrowx, arrowy);
			arrow.addPoint(arrowx - 5, arrowy - 5);
			arrow.addPoint(arrowx - 5, arrowy + 5);
			g2d.fillPolygon(arrow);
			g2d.setTransform(originalTransform);
		}
	}

	// determines which state was clicked
	// returns null if didn't click anything
	public State getClickedState(Point p) {
		Shape bounds;
		for (State tempState : fa.getStates()) {
			bounds = tempState.getEllipse((Graphics2D)this.getGraphics());
			if (bounds.contains(p)) {
				return tempState;
			}
		}
		return null;
	}

	// determines which Transition was clicked
	// returns null if didn't click anything
	private TransitionDot getClickedTransitionDot(Point p) {
		for(Transition nextTransition : fa.getTransitions()) {
			TransitionDot dot = nextTransition.getDot();
			if(dot.getEllipse().contains(p))
				return dot;
		}
		return null;
	}
	
	private boolean isOptionClick(MouseEvent e) {
		return e.getButton() != 1 || e.isPopupTrigger() || (e.isControlDown() && e.getButton() == 1);
	}
	
	// the popup/context menu. depending on what the user clicked on, the
	// appropriate menu will be shown
	public void maybeShowPopup(MouseEvent e) {
		if (isOptionClick(e)) {
			popupState = getClickedState(e.getPoint());
			if (popupState != null) { // clicked on a state
				startStateMenuItem.setVisible(fa.getStartState() != popupState);
				acceptingStateMenuItem.setSelected(popupState.isAccepting());
				statePopupMenu.show(e.getComponent(), e.getX(), e.getY());
			}
			else { // or did we right click on a transition?
				popupTransDot = this.getClickedTransitionDot(e.getPoint());
	
				if (popupTransDot != null) {
					Transition popupTransition = popupTransDot.getTransition();
					
					if(type == Project.TYPE_RFA) {
						for(Transition tempTransition : fa.getTransitions()) {	
							if(popupTransition.equals(tempTransition)) {
								continue;
							}
							else if(tempTransition.getDest().equals(popupTransition.getSrc()) && tempTransition.getSrc().equals(popupTransition.getDest()) 
									&& !popupTransition.isSelfPointing() && !tempTransition.isSelfPointing()) {
								this.reverseMenuItem.setVisible(false);
								this.transitionPopupMenu.show(e.getComponent(), e.getX(), e.getY());
								return;
							}
						}
					}

					this.reverseMenuItem.setVisible(!popupTransDot.getTransition().getSrc().equals(popupTransDot.getTransition().getDest()));
					this.transitionPopupMenu.show(e.getComponent(), e.getX(), e.getY());
					if (type == Project.TYPE_FA)
						this.viewTreeMenuItem.setVisible(false);
				}
			}
		}
	}

	private boolean editLabels(Transition transition) {
		if (type == Project.TYPE_FA) {
			EditLabelsDialog labelsDialog = new EditLabelsDialog(JForlan.getMainFrame(), transition);
			labelsDialog.setVisible(true);
		}
		else {
			String newTransitionLabel;
			if (transition.getLabels().isEmpty()) {
				newTransitionLabel = JOptionPane.showInputDialog(this, "Enter Transition Label");
			}
			else
				newTransitionLabel = JOptionPane.showInputDialog(this, "Enter Transition Label", transition.getLabels().get(0));
			
			if (newTransitionLabel != null) {
				newTransitionLabel = Util.simplifyString(newTransitionLabel);
				if (!newTransitionLabel.equals("")) {
					transition.getLabels().removeAllElements();
					transition.getLabels().add(newTransitionLabel);
				}
			}
		}
		return !transition.getLabels().isEmpty();
	}

	private void editStateName(State state) {
		String label = JOptionPane.showInputDialog(this, "Enter State Name", state.getLabel());
		
		if(label != null) {
			label = Util.simplifyString(label);
			if (!label.equals("")) {
				state.setLabel(label);
			}
		}
		this.repaint();	
	}
		
	public FA getFA() {
		return fa;
	}
	
	public void updateErrors() {
		errors.resetErrors();
		MainFrame mainFrame = JForlan.getMainFrame();

		for(State state : fa.getStates()) {
			ValidityCheck symbolCheck = Controller.getForlanInterface().checkSymbolValidity(state.getLabel());
			if (!symbolCheck.isValid())
				errors.addError(new Error(state, "Invalid symbol."));
		}
		
		Vector<String> stateLabels = new Vector<String>();
		for (State state : fa.getStates()) {
			if (stateLabels.contains(state.getLabel())) {
				errors.addError(new Error(state, "Duplicate state label."));
			}
			else
				stateLabels.add(state.getLabel());
		}

		if (fa.getStartState() == null) {
			errors.addError(new Error(null, "Start state does not exist."));
		}

		for (Transition transition : fa.getTransitions()) {
			ValidityCheck transitionCheck;

			if (type == TYPE_FA) {				
				Vector<String> invalidLabels = new Vector<String>();
				for (String label : transition.getLabels()) {
					transitionCheck = Controller.getForlanInterface().checkStringValidity(label);
					if (!transitionCheck.isValid()) {
						invalidLabels.add(label);
					}
				}
				if (invalidLabels.size() == 1) {
					errors.addError(new Error(transition, "Invalid string: " + invalidLabels.get(0) + "."));
				}
				else if (invalidLabels.size() > 1) {
					String errorString = invalidLabels.toString();
					errorString = errorString.substring(1, errorString.length()-1);
					errors.addError(new Error(transition, "Invalid strings: " + errorString + "."));
				}
			}
			else {
				transitionCheck = Controller.getForlanInterface().checkRegValidity(transition.getLabels().get(0));
				if (!transitionCheck.isValid()) {
					errors.addError(new Error(transition, "Invalid regular expression."));
				}
			}
		}
		
		mainFrame.clearPrintArea();
		if (errors.isEmpty()) {
			String faString = fa.toForlanString();
			if (type == TYPE_FA) {
				faString = Controller.getForlanInterface().getPrettyFA(faString);
			} else {
				faString = Controller.getForlanInterface().getPrettyRFA(faString);
			}
			mainFrame.printLines(faString);
		} else {
			errors.printErrors();
		}
	}
		
	@Override
	public Rectangle calculateImageBounds() {
		
		int top = 0;
		int bottom = 0;
		int left = 0;
		int right = 0;
		
		int tempRight = 0;
		int tempLeft = 0;
		int tempTop = 0;
		int tempBottom = 0;
		
		if(fa.getStates().size() > 0) {
			State state = (State)fa.getStates().get(0);
			right = state.getX() + (int)state.getEllipse((Graphics2D)this.getGraphics()).getWidth()/2;
			left = state.getX() - (int)state.getEllipse((Graphics2D)this.getGraphics()).getWidth()/2;
			top = state.getY() - (int)state.getEllipse((Graphics2D)this.getGraphics()).getHeight()/2;
			bottom = state.getY() + (int)state.getEllipse((Graphics2D)this.getGraphics()).getHeight()/2;
			
			if(fa.getStartState().equals(state))
				left = left - 45;
		}
		for(int i = 1; i < fa.getStates().size(); i++) {
			State state = fa.getStates().get(i);
			tempRight = state.getX() + (int)state.getEllipse((Graphics2D)this.getGraphics()).getWidth()/2;
			tempLeft = state.getX() - (int)state.getEllipse((Graphics2D)this.getGraphics()).getWidth()/2;
			tempTop = state.getY() - (int)state.getEllipse((Graphics2D)this.getGraphics()).getHeight()/2;
			tempBottom = state.getY() + (int)state.getEllipse((Graphics2D)this.getGraphics()).getHeight()/2;
			
			if(fa.getStartState().equals(state))
				tempLeft = tempLeft - 50;
			
			if(tempTop < top)
				top = tempTop;
			if(tempBottom > bottom)
				bottom = tempBottom;
			if(tempLeft < left)
				left = tempLeft;
			if(tempRight > right)
				right = tempRight;
		}

		for(int i = 0; i < fa.getTransitions().size(); i++) {
			Transition transition = fa.getTransitions().get(i);

			if(transition.isSelfPointing()) {
				TransitionDot dot = transition.getDot();
				tempRight = dot.getX() + (int)dot.getEllipse().getWidth()/2;
				tempLeft = dot.getX() - (int)dot.getEllipse().getWidth()/2;
				tempTop = dot.getY() - (int)dot.getEllipse().getHeight()/2;
				tempBottom = dot.getY() + (int)dot.getEllipse().getHeight()/2;
			}
			else {
				QuadCurve2D.Float curve = ShapeUtil.getTransitionLine(transition, (Graphics2D)(this).getGraphics(), false, false);
				Rectangle rect = ShapeUtil.getCurveBounds(curve);
				tempLeft = rect.x;
				tempRight = rect.x+rect.width;
				tempTop = rect.y;
				tempBottom = rect.y+rect.height;
			}
			
			if(tempTop < top)
				top = tempTop;
			if(tempBottom > bottom)
				bottom = tempBottom;
			if(tempLeft < left)
				left = tempLeft;
			if(tempRight > right)
				right = tempRight;
		}
		
		for(Rectangle labelRect : labelBounds) {
			tempLeft = labelRect.x;
			tempRight = labelRect.x+labelRect.width;
			tempTop = labelRect.y;
			tempBottom = labelRect.y+labelRect.height;
			
			if(tempTop < top)
				top = tempTop;
			if(tempBottom > bottom)
				bottom = tempBottom;
			if(tempLeft < left)
				left = tempLeft;
			if(tempRight > right)
				right = tempRight;
		}
		
		left -=5;
		right+=5;
		top-=5;
		bottom+=5;
		
		return new Rectangle(left, top, right-left, bottom - top);
	}
	
	public void resetMouseListeners() {
		for(MouseListener listener : this.getMouseListeners())
			this.removeMouseListener(listener);
		
		for(MouseMotionListener listener : this.getMouseMotionListeners())
			this.removeMouseMotionListener(listener);
		
		this.addMouseListener(new FAPanelMouseAdapter(this));
		this.addMouseMotionListener(new FAPanelMouseMotionAdapter());
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == this.editTransMenuItem) {
			Transition t = popupTransDot.getTransition();
			if (!editLabels(t)) {
				fa.getTransitions().remove(t);
			}
			updateErrors();
			popupTransDot = null;
		}
		else if(e.getSource() == this.removeTransMenuItem) {
			fa.getTransitions().remove(popupTransDot.getTransition());
			updateErrors();
			popupTransDot = null;
		}
		else if(e.getSource() == this.reverseMenuItem) {
			//Transition t = null;
			Transition reversedTransition = popupTransDot.getTransition();
			// check for an existing arrow in the opposite direction
			
			Transition otherTransition = null;
			for(Transition tempTransition : fa.getTransitions()) {	
				if(reversedTransition.equals(tempTransition)) {
					continue;
				}
				else if(tempTransition.getDest().equals(reversedTransition.getSrc()) && tempTransition.getSrc().equals(reversedTransition.getDest()) 
						&& !reversedTransition.isSelfPointing() && !tempTransition.isSelfPointing()) {
					otherTransition = tempTransition;
					break;
				}
			}
			if(otherTransition != null) {
				Vector<String> labels = otherTransition.getLabels();
				for(String label : reversedTransition.getLabels()) {
					if(!labels.contains(label))
						labels.add(label);
				}
				fa.getTransitions().remove(reversedTransition);
			}
			else {
				// do reversal
				State s = reversedTransition.getSrc();
				reversedTransition.setSrc(popupTransDot.getTransition().getDest());
				reversedTransition.setDest(s);
			}
			updateErrors();
			popupTransDot = null;
		}
		else if(e.getSource() == viewTreeMenuItem) {
			JForlan.getMainFrame().getController().viewTreeAsNewProject(popupTransDot.getTransition().getLabels().get(0));
			popupTransDot = null;
		}
		else if(e.getSource() == standardizeMenuItem) {
			popupTransDot.getTransition().standardizeTransitionDot();
			popupTransDot = null;
		}
		else if(e.getSource() == acceptingStateMenuItem) {
			popupState.setAccepting(acceptingStateMenuItem.getState());
			updateErrors();
			popupState = null;
		}
		else if(e.getSource() == startStateMenuItem) {
			fa.setStartState(popupState);
			updateErrors();
			popupState = null;
		}
		else if(e.getSource() == this.editStateMenuItem) {
			editStateName(popupState);
			updateErrors();
			popupState = null;
		}
		else if(e.getSource() == this.removeMenuItem) {
			Vector<Transition> deleteTransitions = new Vector<Transition>();
			for (Transition temp : fa.getTransitions()) {
				if (temp.getSrc().equals(popupState) || temp.getDest().equals(popupState)) {
					deleteTransitions.addElement(temp);
				}
			}
			for (Transition tempTransition : deleteTransitions) {
				fa.getTransitions().removeElement(tempTransition);
			}
			// if this state is the start state...
			if (fa.getStartState() != null && fa.getStartState().equals(popupState)) {
				fa.setStartState(null);
			}
			// remove the state
			fa.getStates().remove(popupState);
			updateErrors();
			popupState = null;
		}
		repaint();
	}

	private class FAPanelMouseMotionAdapter extends MouseMotionAdapter {
		@Override
		public void mouseDragged(MouseEvent e) {
			int x = e.getPoint().x;
			int y = e.getPoint().y;
			if (currentDraggedState != null) {
				currentDraggedState.setX(x);
				currentDraggedState.setY(y);
				
				// keep states from having negative coordinates
				if (currentDraggedState.getX() - 25 < 0)
					currentDraggedState.setX(25);
				if (currentDraggedState.getY() - 25 < 0)
					currentDraggedState.setY(25);
				if(currentDraggedState.getX() + 25 > getWidth())
					currentDraggedState.setX(getWidth()-25);
				if(currentDraggedState.getY() + 25 > getHeight())
					currentDraggedState.setY(getHeight()-25);
				repaint();
			}
			if (srcStateClicked != null) {
				destPoint.move(x, y);
				repaint();
			}
			if (currentDraggedTransitionDot != null) {
				currentDraggedTransitionDot.setLocation(e.getPoint());
				currentDraggedTransitionDot.getTransition().setCustomized(true);
				repaint();
			}
		}
	}
	
	private class FAPanelMouseAdapter extends MouseAdapter {
		FAProject faProject;

		public FAPanelMouseAdapter(FAProject faProject) {
			super();
			this.faProject = faProject;
		}
		@Override
		public void mousePressed(MouseEvent e) {
			boolean stateMode = JForlan.getMainFrame().isStateMode();
			boolean transMode = !stateMode;

			if (popupTransDot != null) { // canceling transition popup
				popupTransDot = null;
			} else if (popupState != null) { // canceling state popup
				popupState = null;
			} else if (isOptionClick(e)) {
				maybeShowPopup(e);
			}
			// Grab a state for dragging or create a new one
			else if (stateMode && e.getButton() == MouseEvent.BUTTON1) {
				currentDraggedState = getClickedState(e.getPoint());
				if (currentDraggedState != null) { // we clicked on a state, prep for drag
					fa.getStates().removeElement(currentDraggedState); //this keeps the dragged state on top
					fa.getStates().addElement(currentDraggedState);
					repaint();
				}
				else { // didn't click on a state, so create a new one
					String label = JOptionPane.showInputDialog(faProject, "Enter State Name");
					if(label != null) {
						label = Util.simplifyString(label);
						if (!label.equals("")) {
							State state = new State(label, e.getPoint().x, e.getPoint().y);
							fa.getStates().add(state);
							if (fa.getStates().size() == 1) {
								// make first state the start state
								fa.setStartState(state);
							}
							updateErrors();
						}
					}
					repaint();
				}
			}
			else if (transMode && e.getButton() == MouseEvent.BUTTON1) {
				srcStateClicked = getClickedState(e.getPoint());
				if (srcStateClicked == null) {
					currentDraggedTransitionDot = getClickedTransitionDot(e.getPoint());
				}
				else {
					destPoint = e.getPoint();
				}
			}
		}

		public void mouseReleased(MouseEvent e) {
			if (currentDraggedState != null) { 				
				currentDraggedState = null;
				repaint();
			}
			else if (currentDraggedTransitionDot != null) {
				currentDraggedTransitionDot = null;
				repaint();
			}
			else if (srcStateClicked != null) {
				State srcState = srcStateClicked;
				State destState = getClickedState(destPoint);
				srcStateClicked = null;
				if (destState != null) { // add a transition
					Transition t = null;

					// is this transition already there?
					for (Transition temp : fa.getTransitions()) {
						if (temp.getSrc().equals(srcState) && temp.getDest().equals(destState)) {
							t = temp;
							break;
						}
					}

					if (t == null) {
						t = new Transition(srcState, destState);
						fa.getTransitions().add(t);
					}

					if (!editLabels(t)) {
						fa.getTransitions().remove(t);
					}

					updateErrors();
				}
				repaint();
			}
		}
	}
}
