package com.wilsonvillerobotics.firstteamscouter;

import android.graphics.Point;
import android.widget.ImageView;

/**
 * Created by SommervilleT on 2/9/2015.
 */
public class GameElement {
    private ImageView   elementImageView;
    private Point       elementLocation;
    private boolean     elementVisible;
    private int         elementId;

    GameElement() {
        this.elementImageView = null;
        this.elementLocation = null;
        this.elementId = -1;
        this.elementVisible = false;
    }

    GameElement(int id, ImageView iv, Point loc, boolean vis) {
        this.elementId = id;
        this.elementImageView = iv;
        this.elementLocation = loc;
        this.elementVisible = vis;
    }

    public int getId() {
        return this.elementId;
    }

    public void setId(int id) {
        this.elementId = id;
    }

    public boolean isVisible() {
        return this.elementVisible;
    }

    public void makeVisible() {
        this.elementVisible = true;
    }

    public void makeInvisible() {
        this.elementVisible = false;
    }

    public ImageView getImageView() {
        return this.elementImageView;
    }

    public void setImageView(ImageView iv) {
        this.elementImageView = iv;
    }

    public Point getLocation() {
        return this.elementLocation;
    }

    public void setLocation(Point p) {
        this.elementLocation = p;
    }

    public void setLocation(int x, int y) {
        this.elementLocation = new Point(x, y);
    }
}
