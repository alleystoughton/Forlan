package jforlan.automata;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.awt.Point;
import java.awt.geom.Ellipse2D;
import java.io.Serializable;

public class TransitionDot implements Serializable {

	private static final long serialVersionUID = 1L;
    
	int x;
	int y;
	Transition transition;
	
	public TransitionDot(Transition transition, int x, int y) {
		this.transition = transition;
		this.x = x;
		this.y = y;
	}

	public int getX() {
		return x;
	}

	public void setX(int x) {
		this.x = x;
	}

	public int getY() {
		return y;
	}

	public void setY(int y) {
		this.y = y;
	}
	
	public Ellipse2D.Float getEllipse() {
			return new Ellipse2D.Float(x-4, y-4, 8, 8);
	}
	
	public void setLocation(Point p) {
		x = (int)p.getX();
		y = (int)p.getY();
	}
	
	public Transition getTransition() {
		return transition;
	}
}
