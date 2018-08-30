package jforlan.utils;

/* The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. */

import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.QuadCurve2D;

import jforlan.automata.Arrow;
import jforlan.automata.State;
import jforlan.automata.Transition;
import jforlan.automata.TransitionDot;
import jforlan.trees.Node;

public class ShapeUtil {
	
	public static Point2D getNearestPointOnShape(double px, double py, Shape s, AffineTransform tfx) {
		PathIterator pit = s.getPathIterator(tfx, .01);
	
		double[] seg = new double[6];
		//[0] = current x
		//[1] = current y
		//[2] = previous x
		//[3] = previous y
		//[4] = last moveTo x
		//[5] = last moveTo y
		
		Point2D.Double closest = new Point2D.Double();
		double currentMin = Double.MAX_VALUE;
		
		while (!pit.isDone()) {
			int retVal = pit.currentSegment(seg);
			
			if (retVal == PathIterator.SEG_MOVETO) {
				seg[4] = seg[2] = seg[0];
				seg[5] = seg[3] = seg[1];
			} 
			else if (retVal == PathIterator.SEG_CLOSE) {
				seg[2] = seg[4];
				seg[3] = seg[5];
			}
			
			if (seg[0] == seg[2] && seg[1] == seg[3]) {
				pit.next();
				continue;
			}
			
			//vector y (the point)
			double y1 = px - seg[2];
			double y2 = py - seg[3];
			
			//vector u (the line segment)
			double u1 = seg[0] - seg[2];
			double u2 = seg[1] - seg[3];
			
			//orthogonal projection of y onto u
			double scale = (y1 * u1 + y2 * u2) / (u1 * u1 + u2 * u2);
			double projX, projY;
			
			if (scale < 0) {
				projX = 0;
				projY = 0;
			} 
			else if (scale > 1) {
				projX = u1;
				projY = u2;
			} 
			else {
				projX = scale * u1;
				projY = scale * u2;
			}
	
			//squared distance between point and projected point
			double sqDistToProj = (projX - y1) * (projX - y1) + (projY - y2) * (projY - y2);
			if (sqDistToProj < currentMin) {
				
				closest.x = projX + seg[2];
				closest.y = projY + seg[3];
				
				currentMin = sqDistToProj;
			}
			
			seg[2] = seg[0];
			seg[3] = seg[1];
			pit.next();
		}
		
		return closest;
	}
	
	public static double getDistanceBetweenPoints(Point2D p1, Point2D p2) {
		double x_dist_squared = (p2.getX() - p1.getX())*(p2.getX() - p1.getX());
		double y_dist_squared = (p2.getY() - p1.getY())*(p2.getY() - p1.getY());
		return Math.sqrt(x_dist_squared + y_dist_squared);
	}
	
	public static Point2D getBezierControlPoint(Point2D start, Point2D end, Point2D through) {
		double dist_to_start = getDistanceBetweenPoints(through, start);
		double dist_to_end = getDistanceBetweenPoints(through, end);
		double total_dist = dist_to_start + dist_to_end;

		double arc = 1 - dist_to_end/total_dist;

		double left_of_divisor = through.getX() - (1 - arc)*(1-arc)*start.getX() - arc*arc*end.getX();
		double right_of_divisor = 2 * (1 - arc) * arc;
		double control_x = left_of_divisor / right_of_divisor;
		
		left_of_divisor = through.getY() - (1 - arc)*(1-arc)*start.getY() - arc*arc*end.getY();
		right_of_divisor = 2 * (1 - arc) * arc;
		double control_y = left_of_divisor / right_of_divisor;
		
		return new Point((int)control_x, (int)control_y);
	}
	
	public static QuadCurve2D.Float getTransitionLine(Transition transition, Graphics2D g2d, boolean bidirectional, boolean secondDrawn) {
		
		if(!transition.isCustomized())
			transition.standardizeTransitionDot();

		Point srcPoint = new Point(transition.getSrc().getX(), transition.getSrc().getY());
		Point destPoint = new Point(transition.getDest().getX(), transition.getDest().getY());
		
		if(bidirectional && !transition.isCustomized()) {	
			double dx = (transition.getDest().getX() - transition.getSrc().getX());
			double dy = (transition.getDest().getY() - transition.getSrc().getY());
			double total = Math.abs(dx) + Math.abs(dy);
			 
			double x_offset = 13* dy/total;
			double y_offset = -13* dx/total;
			if(secondDrawn) {
				srcPoint.setLocation(srcPoint.getX() + x_offset,srcPoint.getY() + y_offset);
				destPoint.setLocation(destPoint.getX() + x_offset, destPoint.getY() + y_offset);
				transition.getDot().setX(transition.getDot().getX() + (int)x_offset);
				transition.getDot().setY(transition.getDot().getY() + (int)y_offset);		
			}
			else {
				srcPoint.setLocation(srcPoint.getX() + x_offset, srcPoint.getY() + y_offset);
				destPoint.setLocation(destPoint.getX() + x_offset, destPoint.getY() + y_offset);
				transition.getDot().setX(transition.getDot().getX() + (int)x_offset);
				transition.getDot().setY(transition.getDot().getY() + (int)y_offset);
			}
		}
		else {
			srcPoint = new Point(transition.getSrc().getX(), transition.getSrc().getY());
			destPoint = new Point(transition.getDest().getX(), transition.getDest().getY());
		}
		
		Point2D controlPoint = getBezierControlPoint(srcPoint, destPoint, new Point(transition.getDot().getX(), transition.getDot().getY()));

		return new QuadCurve2D.Float((float)srcPoint.getX(), (float)srcPoint.getY(), (float)controlPoint.getX(), (float)controlPoint.getY(), (float)destPoint.getX(), (float)destPoint.getY());
	}
		
	public static Ellipse2D.Float getNonRotatedSelfTransitionLine(Transition transition, Graphics2D g2d) {
		
		if(!transition.isCustomized()) {
			transition.standardizeTransitionDot();
		}
		
		State state = transition.getSrc();
		TransitionDot dot = transition.getDot();
		
		double diameter = getDistanceBetweenPoints(new Point(state.getX(), state.getY()), new Point(dot.getX(), dot.getY()));
		Ellipse2D.Float transEllipse = new Ellipse2D.Float((float)state.getX()+10,(float)state.getY() - 20, (float)diameter -10,(float) 40);
		
		return transEllipse;
	}
	
	public static Ellipse2D.Float getEllipse(State state, Graphics2D g2d) {
		
		FontMetrics fontMetrics = g2d.getFontMetrics();
		int width;
		if(state.getLabel() != null)
		    width = fontMetrics.stringWidth(state.getLabel());
		else
			width = 0;
			
		return new Ellipse2D.Float(state.getX() - Math.max(20,((width+20)/2)), state.getY() - 20, Math.max(40, width+20), 40);
	}
	
	public static Ellipse2D.Float getEllipse(Node node, Graphics2D g2d) {
		FontMetrics fontMetrics = g2d.getFontMetrics();
		int width = fontMetrics.stringWidth(node.getLabel());
			
		return new Ellipse2D.Float(node.getX() - Math.max(20,((width+20)/2)), node.getY() - 20, Math.max(40, width+20), 40);
	}
	
	
	public static Ellipse2D.Float getAcceptingEllipse(State state, Graphics2D g2d) {
		FontMetrics fontMetrics = g2d.getFontMetrics();
		int width = fontMetrics.stringWidth(state.getLabel());
			
		if(state.isAccepting()) {
			return new Ellipse2D.Float(state.getX() - Math.max(17,((width+14)/2)), state.getY() - 17, Math.max(34, width+14), 34);
		}
		else
			return null;
	}
	
	public static Point getMidPoint(int x1, int y1, int x2, int y2) {
		return new Point((x2+x1)/2, (y2+y1)/2);
	}
	
	 public static double getAngleBetweenPoints(Point p1, Point p2) {
		 return Math.atan2(p2.getY() - p1.getY(), p2.getX()-p1.getX()); 
	 }
 
	 public static Arrow getTransitionArrow(Transition transition, boolean bidirectional, QuadCurve2D.Float curve, Graphics2D g2d) {
		 
		 Point nearestPoint = new Point(0,0);
		 
		 if((bidirectional && !transition.isCustomized()) || ShapeUtil.isCurveStraight(curve)) {
			 Ellipse2D.Float destEllipse = transition.getDest().getEllipse(g2d);
			 
			 double diameter = getDistanceBetweenPoints(curve.getP1(), curve.getP2());
			 int numPoints = (int)Math.ceil(diameter/3);
			 double dx = (curve.getP2().getX() - curve.getP1().getX())/numPoints;
			 double dy = (curve.getP2().getY() - curve.getP1().getY())/numPoints;
			 
			 double x = curve.getP1().getX();
			 double y = curve.getP1().getY();
			 
			 Point currentPoint = new Point(0,0);
			 for(int i = 1; i < numPoints; i++) {
				 nearestPoint = currentPoint;
				 currentPoint = new Point((int)(x+dx*i), (int)(y+dy*i));
				 //Ellipse2D.Float ellipse = new Ellipse2D.Float((int)currentPoint.getX(), (int)currentPoint.getY(), 3, 3);
				 //g2d.fill(ellipse);
				 if(destEllipse.contains(currentPoint)) {
					break;
				 }
			 }
		 }
		 else { 
			 Point startPoint = new Point((int)curve.getP1().getX(), (int)curve.getP1().getY());
			 
			 Point closePoint = getCurvePointCloseToShape(startPoint, curve,  transition.getDest().getEllipse(g2d), g2d);
			 Point2D nearestDstPoint = getNearestPointOnShape(closePoint.getX(), closePoint.getY(), transition.getDest().getEllipse(g2d), null);
			 nearestPoint = new Point((int)nearestDstPoint.getX(), (int)nearestDstPoint.getY());
		 }
		 
		 Polygon arrowPoly = new Polygon();
		 arrowPoly.addPoint((int)nearestPoint.getX()+1, (int)nearestPoint.getY());
		 arrowPoly.addPoint((int)nearestPoint.getX()+1 - 7, (int)nearestPoint.getY() + 7);
		 arrowPoly.addPoint((int)nearestPoint.getX()+1 - 7, (int)nearestPoint.getY() - 7);
		 
		 double angle = 0;
		 if(bidirectional && !transition.isCustomized()) {
			 Point p1 = new Point((int)curve.getP1().getX(), (int)curve.getP1().getY());
			 Point p2 = new Point((int)curve.getP2().getX(), (int)curve.getP2().getY());
			 angle = getAngleBetweenPoints(p1, p2);
		 }
		 else {
		 	angle = getAngleBetweenPoints(nearestPoint, new Point(transition.getDest().getX(), transition.getDest().getY())); //changed from closePoint
		 }
		 return new Arrow(nearestPoint, angle, arrowPoly);
	 }
	 
	 public static Arrow getSelfTransitionArrow(Transition transition, Shape curve, Graphics2D g2d) {
		 Point startPoint = new Point(transition.getDot().getX(), transition.getDot().getY());

		 Point closePoint = getCurvePointCloseToShape(startPoint, curve,  transition.getDest().getEllipse(g2d), g2d);
		 Point2D nearestDstPoint = getNearestPointOnShape(closePoint.getX(), closePoint.getY(), transition.getDest().getEllipse(g2d), null);
		 Point nearestPoint = new Point((int)nearestDstPoint.getX(), (int)nearestDstPoint.getY());
		 //System.out.println("close point: " + closePoint + "     nearestDestPoint: " + nearestDstPoint);
			 
		 Polygon arrowPoly = new Polygon();
		 
		 arrowPoly.addPoint((int)nearestDstPoint.getX()+1, (int)nearestDstPoint.getY());
		 arrowPoly.addPoint((int)nearestDstPoint.getX()+1 - 7, (int)nearestDstPoint.getY() + 7);
		 arrowPoly.addPoint((int)nearestDstPoint.getX()+1 - 7, (int)nearestDstPoint.getY() - 7);
			 
		 //double angle = getAngleBetweenPoints(new Point((int)nearestDstPoint.getX(), (int)nearestDstPoint.getY()), closePoint);
		 double angle = getAngleBetweenPoints(closePoint, new Point(transition.getDest().getX(), transition.getDest().getY()));

		 return new Arrow(nearestPoint, angle, arrowPoly);
	 }
	 
	 public static Arrow getNodeArrow(Node child, Node parent, Graphics2D g2d) {
		 if(child.getParent() == null)
			 return null;
		 
		 
		 Point2D nearestDstPoint = ShapeUtil.getNearestPointOnShape(child.getX(), child.getY(), parent.getEllipse(g2d), null);
			 
		 Polygon arrowPoly = new Polygon();
		 arrowPoly.addPoint((int)nearestDstPoint.getX(), (int)nearestDstPoint.getY());
		 arrowPoly.addPoint((int)nearestDstPoint.getX() - 5, (int)nearestDstPoint.getY() + 5);
		 arrowPoly.addPoint((int)nearestDstPoint.getX() - 5, (int)nearestDstPoint.getY() - 5);
		 
		 double angle =  ShapeUtil.getAngleBetweenPoints(new Point(child.getX(), child.getY()), new Point((int)nearestDstPoint.getX(), (int)nearestDstPoint.getY()));
		 return new Arrow(nearestDstPoint, angle, arrowPoly);
	 }
	  
	 public static Point getCurvePointCloseToShape(Point startPoint, Shape line, Shape shape, Graphics2D g2d) {
		 PathIterator iter = line.getPathIterator(null, 0.01);
			
		 int size = 0;
		 while(!iter.isDone()) {
			 size++;
			 iter.next();
		 }
		 
		 if(size < 10) {
			 Point2D point =  getNearestPointOnShape(startPoint.getX(), startPoint.getY(), shape, null);
			 return new Point((int)point.getX(), (int)point.getY());
		 }
		 
		 double[] lastSeg = new double[6];
		 PathIterator secondIter = line.getPathIterator(null, 0.01);
		 secondIter.currentSegment(lastSeg);
		 
		 secondIter.next();
		 if(!secondIter.isDone()) {

			 for(int i = 0; i < size -1; i++) {
				 secondIter.currentSegment(lastSeg);
				 
				 if(shape.contains(new Point((int)lastSeg[0], (int)lastSeg[1]))) {
					 break;
				 }
				 secondIter.next();
			 }
		 }
		 return new Point((int)lastSeg[0], (int)lastSeg[1]);
	 }
	 
	 public static boolean isRectNearCurve(QuadCurve2D.Float line, Rectangle rect, Graphics2D g2d) {
		 PathIterator iter = line.getPathIterator(null, .3);
			
		 int size = 0;
		 while(!iter.isDone()) {
			 size++;
			 iter.next();
		 }
		 //g2d.setColor(Color.MAGENTA);
		 if(size < 7) {
			 double diameter = getDistanceBetweenPoints(line.getP1(), line.getP2());
			 int numPoints = (int)Math.ceil(diameter/20);
			 double dx = (line.getP2().getX() - line.getP1().getX())/numPoints;
			 double dy = (line.getP2().getY() - line.getP1().getY())/numPoints;
			 
			 double x = line.getP1().getX();
			 double y = line.getP1().getY();
			 for(int i = 1; i < numPoints; i++) {
				 //Ellipse2D.Float ellipse = new Ellipse2D.Float((int)(x+dx*i), (int)(y+dy*i), 2, 2);
				 //g2d.draw(ellipse);
				 if(rect.contains(new Point((int)(x+dx*i), (int)(y+dy*i))))
					 return true;
			 }
			 return false;
		 }

		 double[] lastSeg = new double[2];
		 PathIterator secondIter = line.getPathIterator(null, .3);
		 
		 secondIter.currentSegment(lastSeg);
		 
		 secondIter.next();
		 if(!secondIter.isDone()) {

			 for(int i = 0; i < Math.floor(size/1.01); i++) { //Can't use the last point, best or work hack ever?! :-D
				 secondIter.currentSegment(lastSeg);
				// Ellipse2D.Float ellipse = new Ellipse2D.Float((int)lastSeg[0], (int)lastSeg[1], 2, 2);
				// g2d.draw(ellipse);
				 if(rect.contains(new Point((int)lastSeg[0], (int)lastSeg[1])))
					 return true;
				 secondIter.next();
			 }
		 }
		 return false;
	 }
	 
	 private static boolean isCurveStraight(QuadCurve2D.Float curve) {
		 PathIterator iter = curve.getPathIterator(null, 0.3);
			
		 int size = 0;
		 while(!iter.isDone()) {
			 iter.next();
			 size++;
			 if(size > 10) {
				 return false;
			 }
		 }
		 return true;
	 }
	 
	 public static Rectangle getCurveBounds(QuadCurve2D.Float curve) {
		 PathIterator iter = curve.getPathIterator(null, .01);
		
		 int left = 0;
		 int right = 0;
		 int top = 0;
		 int bottom = 0;
		 
		 int size = 0;
		 while(!iter.isDone()) {
			 size++;
			 iter.next();
		 }
		 
		 double[] lastSeg = new double[6];
		 PathIterator secondIter = curve.getPathIterator(null, 0.01);
		 secondIter.currentSegment(lastSeg);
		 left = (int)lastSeg[0];
		 right = (int)lastSeg[0];
		 top = (int)lastSeg[1];
		 bottom = (int)lastSeg[1];
		 
		 secondIter.next();
		 if(!secondIter.isDone()) {

			 for(int i = 0; i < size -1; i++) {
				 secondIter.currentSegment(lastSeg);
				 
				 if((int)lastSeg[0] < left)
					 left = (int)lastSeg[0];
				 if((int)lastSeg[0] > right)
					 right = (int)lastSeg[0];
				 if((int)lastSeg[1] < top)
					 top = (int)lastSeg[1];
				 if((int)lastSeg[1] > bottom)
					 bottom = (int)lastSeg[1];
				 secondIter.next();
			 }
		 }
		 return new Rectangle(left, top, right-left, bottom-top);
	 }
}
