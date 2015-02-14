package com.wilsonvillerobotics.firstteamscouter;

import android.graphics.Point;
import android.view.View;
import android.widget.ImageView;

/**
 * Created by SommervilleT on 2/9/2015.
 */
public class GameElement {
    public enum GameElementType {
        ROBOT("ROBOT"),
        GRAY_TOTE("GRAY_TOTE"),
        YELLOW_TOTE("YELLOW_TOTE"),
        CAN("CAN"),
        TRASH("TRASH"),
        UNKNOWN("UNKNOWN");

        private String type;
        GameElementType(String type) {
            this.type = type;
        }

        static GameElementType getTypeByString(String type) {
            for(GameElementType get : GameElementType.values()) {
                if(get.type.equals(type)) {
                    return get;
                }
            }
            return UNKNOWN;
        }
    }

    public enum GameElementState {
        UPRIGHT,
        ONSIDE,
        UPSIDEDOWN,
        ONEND,
        UNKNOWN
    }

    private ImageView           elementImageView;
    private Point               elementLocation;
    private boolean             elementVisible;
    private int                 elementId;
    private GameElement         elementLink;
    private GameElementType     elementType;
    private GameElementState    elementState;
    private boolean             elementActive;

    GameElement() {
        this.elementImageView = null;
        this.elementLocation = null;
        this.elementId = -1;
        this.elementVisible = false;
        this.elementLink = null;
        this.elementType = GameElementType.UNKNOWN;
        this.elementState = GameElementState.UNKNOWN;
        this.elementActive = false;
    }

    GameElement(int id, ImageView iv, Point loc, boolean vis) {
        this.elementId = id;
        this.elementImageView = iv;
        this.elementLocation = loc;
        this.elementVisible = vis;
        this.elementLink = null;
        this.elementType = GameElementType.UNKNOWN;
        this.elementState = GameElementState.UNKNOWN;
        this.elementActive = vis;
    }

    GameElement(int id, ImageView iv, Point loc, boolean vis, GameElementType et, GameElementState ges) {
        this.elementId = id;
        this.elementImageView = iv;
        this.elementLocation = loc;
        this.elementVisible = vis;
        this.elementLink = null;
        this.elementType = et;
        this.elementState = ges;
        this.elementActive = vis;
    }

    public int getId() {
        return this.elementId;
    }

    public void setId(int id) {
        this.elementId = id;
    }

    public GameElementType getElementType() {
        return this.elementType;
    }

    public void setElementType(GameElementType et) {
        this.elementType = et;
    }

    public void setNextElement(GameElement el) {
        this.elementLink = el;
    }

    public GameElement nextElement() {
        return this.elementLink;
    }

    public GameElementState getElementState() {
        return this.elementState;
    }

    public void setElementState(GameElementState state) {
        this.elementState = state;
    }

    public void pushToStack(GameElement el) {
        el.makeInvisible();
        GameElement currElement = this;
        while(currElement.nextElement() != null) {
            currElement = currElement.nextElement();
        }
        currElement.setNextElement(el);
    }

    public GameElement popFromStack() {
        GameElement currElement = this;
        GameElement prevElement = null;
        while(currElement.nextElement() != null) {
            prevElement = currElement;
            currElement = currElement.nextElement();
        }

        if(prevElement == null) return null;

        currElement.makeVisible();
        prevElement.setNextElement(null);
        return currElement;
    }

    public int getStackSize() {
        int count = 1;
        GameElement currElement = this;
        while(currElement.nextElement() != null) {
            count++;
            currElement = currElement.nextElement();
        }
        return count;
    }

    public String getStackList() {
        String stackList = "";
        GameElement currElement = this;
        while(currElement != null) {
            if(!currElement.isRobot()) {
                stackList += currElement.getId() + " ";
            }
            currElement = currElement.nextElement();
        }
        return stackList;
    }

    public boolean isVisible() {
        return this.elementVisible;
    }

    public boolean isActive() {
        return this.elementActive;
    }

    public void activate() {
        this.elementActive = true;
    }

    public void deactivate() {
        this.elementActive = false;
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

    public void setImageViewTag(Object tag) {
        if(this.elementImageView != null) {
            this.elementImageView.setTag(tag);
        }
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

    public boolean isRobot() {
        return this.elementType == GameElementType.ROBOT;
    }

    public boolean isTote() {
        boolean isATote = (this.elementType == GameElementType.GRAY_TOTE) ||
                (this.elementType == GameElementType.YELLOW_TOTE);
        return isATote;
    }

    public boolean isCan() {
        return this.elementType == GameElementType.CAN;
    }

    public boolean isTrash() {
        return this.elementType == GameElementType.TRASH;
    }
}
