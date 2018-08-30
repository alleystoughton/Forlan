package jforlan.automata;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.awt.Polygon;
import java.awt.geom.Point2D;

public class Arrow {
	double angle;
	Point2D location;
	Polygon polygon;
	
	public Arrow(Point2D location, double angle, Polygon polygon) {
		this.location = location;
		this.angle = angle;
		this.polygon = polygon;
	}

	public double getAngle() {
		return angle;
	}

	public void setAngle(double angle) {
		this.angle = angle;
	}

	public Point2D getLocation() {
		return location;
	}

	public void setLocation(Point2D location) {
		this.location = location;
	}

	public Polygon getPolygon() {
		return polygon;
	}

	public void setPolygon(Polygon polygon) {
		this.polygon = polygon;
	}
	
	
}
