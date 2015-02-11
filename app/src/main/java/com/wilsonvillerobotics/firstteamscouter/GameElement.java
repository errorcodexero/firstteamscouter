package com.wilsonvillerobotics.firstteamscouter;

import android.graphics.Point;
import android.view.View;
import android.widget.ImageView;

/**
 * Created by SommervilleT on 2/9/2015.
 */
public class GameElement {
    public enum ElementType {
        ROBOT,
        TOTE,
        CAN,
        TRASH
    }

    private ImageView   elementImageView;
    private Point       elementLocation;
    private boolean     elementVisible;
    private int         elementId;
    private GameElement elementLink;
    public ElementType  elementType;

    GameElement() {
        this.elementImageView = null;
        this.elementLocation = null;
        this.elementId = -1;
        this.elementVisible = false;
        this.elementLink = null;
        this.elementType = null;
    }

    GameElement(int id, ImageView iv, Point loc, boolean vis) {
        this.elementId = id;
        this.elementImageView = iv;
        this.elementLocation = loc;
        this.elementVisible = vis;
        this.elementLink = null;
        this.elementType = null;
    }

    public int getId() {
        return this.elementId;
    }

    public void setId(int id) {
        this.elementId = id;
    }

    public ElementType getElementType() {
        return this.elementType;
    }

    public void setElementType(ElementType et) {
        this.elementType = et;
    }

    public void setNextElement(GameElement el) {
        this.elementLink = el;
    }

    public GameElement getNextElement() {
        return this.elementLink;
    }

    public void pushToStack(GameElement el) {
        el.makeInvisible();
        GameElement currElement = this;
        while(currElement.getNextElement() != null) {
            currElement = currElement.getNextElement();
        }
        currElement.setNextElement(el);
    }

    public GameElement popFromStack() {
        GameElement currElement = this;
        GameElement prevElement = null;
        while(currElement.getNextElement() != null) {
            prevElement = currElement;
            currElement = currElement.getNextElement();
        }

        if(prevElement == null) return null;

        currElement.makeVisible();
        prevElement.setNextElement(null);
        return currElement;
    }

    public int getStackSize() {
        int count = 1;
        GameElement currElement = this;
        while(currElement.getNextElement() != null) {
            count++;
            currElement = currElement.getNextElement();
        }
        return count;
    }

    public boolean isVisible() {
        return this.elementVisible;
    }

    public void setVisibility(boolean visibility) {
        this.elementVisible = visibility;
    }

    public void makeVisible() {
        this.elementVisible = true;
        this.elementImageView.setVisibility(View.VISIBLE);
    }

    public void makeInvisible() {
        this.elementVisible = false;
        this.elementImageView.setVisibility(View.INVISIBLE);
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
