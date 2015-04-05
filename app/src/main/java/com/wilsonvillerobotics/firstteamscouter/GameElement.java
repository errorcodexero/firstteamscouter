package com.wilsonvillerobotics.firstteamscouter;

import android.content.Context;
import android.graphics.Point;
import android.util.AttributeSet;
import android.widget.ImageView;

import java.util.ArrayList;

/**
 * Created by SommervilleT on 2/9/2015.
 */
public class GameElement extends ImageView {
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

        String getType() {
            return this.type;
        }
    }

    public enum GameElementState {
        UPRIGHT ("Upright"),
        ONSIDE ("On Side"),
        UPSIDEDOWN ("Upside Down"),
        ONEND ("On End"),
        UNKNOWN ("Unknown");

        private String state;
        GameElementState(String state) {
            this.state = state;
        }

        static GameElementState getStateByString(String state) {
            for(GameElementState ges : GameElementState.values()) {
                if(ges.state.equals(state)) {
                    return ges;
                }
            }
            return UNKNOWN;
        }

        String getState() {
            return this.state;
        }
    }

    public enum GameElementLocation {
        ROBOT("ROBOT"),
        GROUND("GROUND"),
        FEEDER("FEEDER"),
        STEP("STEP"),
        PLATFORM("PLATFORM"),
        UNKNOWN("UNKNOWN");

        private String location;
        GameElementLocation(String loc) {
            this.location = loc;
        }

        static GameElementLocation getTypeByString(String loc) {
            for(GameElementLocation gel : GameElementLocation.values()) {
                if(gel.location.equals(loc)) {
                    return gel;
                }
            }
            return UNKNOWN;
        }

        String getLocation() {
            return this.location;
        }
    }

    private Point               elementCoordinates;
    private boolean             elementVisible;
    private int                 elementId;
    private GameElement         elementLink;
    private GameElementType     elementType;
    private GameElementState    elementState;
    private GameElementLocation elementLocation;
    private boolean             elementActive;

    private Context context;

    GameElement(Context context) {
        super(context);
        this.context = context;
        this.elementCoordinates = null;
        this.elementId = -1;
        this.elementVisible = false;
        this.elementLink = null;
        this.elementType = GameElementType.UNKNOWN;
        this.elementState = GameElementState.UNKNOWN;
        this.elementActive = false;
    }

    public GameElement(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.context = context;
        this.elementCoordinates = null;
        this.elementId = -1;
        this.elementVisible = false;
        this.elementLink = null;
        this.elementType = GameElementType.UNKNOWN;
        this.elementState = GameElementState.UNKNOWN;
        this.elementActive = false;
    }

    public GameElement(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        this.context = context;
        this.elementCoordinates = null;
        this.elementId = -1;
        this.elementVisible = false;
        this.elementLink = null;
        this.elementType = GameElementType.UNKNOWN;
        this.elementState = GameElementState.UNKNOWN;
        this.elementActive = false;
    }

    ///
    /// Copy Constructor
    ///
    public GameElement(Context context, GameElement ge) {
        super(context);
        this.context = context;
        this.setImageDrawable(ge.getDrawable());
        elementCoordinates = ge.getLocation();
        elementVisible = ge.isVisible();
        elementId = ge.getId();
        elementLink = ge.nextElement();
        elementType = ge.getElementType();
        elementState = ge.getElementState();
        elementLocation = ge.getElementLocation();
        elementActive = ge.isActive();
    }

    public void initGameElement(int id, Point loc, boolean vis, GameElementType get, GameElementState ges, GameElementLocation gel) {
        this.elementId = id;
        this.elementCoordinates = loc;
        this.elementVisible = vis;
        this.elementLink = null;
        this.elementType = get;
        this.elementState = ges;
        this.elementLocation = gel;
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

    public GameElementLocation getElementLocation() { return this.elementLocation; };

    public void setElementLocation(GameElementLocation gel) {
        this.elementLocation = gel;
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

    public GameElement getElementAtIndex(int index) {
        GameElement currElement = this.nextElement();
        GameElement prevElement = this;
        int i = 0;
        while(currElement != null) {
            prevElement = currElement;
            currElement = currElement.nextElement();
            if(i == index) break;
            i++;
        }

        if(i == index) {
            if (currElement != null) {
                currElement.makeVisible();
                prevElement.setNextElement(currElement.nextElement());
                currElement.setNextElement(null);
                return currElement;
            }
        }
        return null;
    }

    public GameElement peekAtIndex(int index) {
        GameElement currElement = this.nextElement();
        int i = 0;
        while(currElement != null) {
            currElement = currElement.nextElement();
            if(i == index) break;
            i++;
        }
        return currElement;
    }

    public ArrayList<GameElement> getAllOfTypeFromStack(GameElementType type) {
        ArrayList<GameElement> elements = new ArrayList<GameElement>();
        GameElement currElement = this.nextElement();
        GameElement prevElement = this;
        GameElement nextElement = null;
        while(currElement != null) {
            nextElement = currElement.nextElement();
            if(currElement.getElementType() == type) {
                prevElement.setNextElement(nextElement);
                currElement.setNextElement(null);
                elements.add(currElement);
                currElement = nextElement;
            } else {
                prevElement = currElement;
                currElement = nextElement;
            }
        }
        return elements;
    }

    public boolean stackHas(GameElementType type) {
        GameElement currElement = this;
        boolean cansFound = false;
        while(currElement != null) {
            cansFound = (currElement.getElementType() == type);
            if(cansFound) break;
            currElement = currElement.nextElement();
        }
        return cansFound;
    }

    public int getStackSize() {
        int count = 0;
        GameElement currElement = this.nextElement();
        while(currElement != null) {
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
        this.setVisibility(VISIBLE);
        //this.elementImageView.setVisibility(View.VISIBLE);
    }

    public void makeInvisible() {
        this.elementVisible = false;
        this.setVisibility(INVISIBLE);
        //this.elementImageView.setVisibility(View.INVISIBLE);
    }

    public Point getLocation() {
        return this.elementCoordinates;
    }

    public void setLocation(Point p) {
        this.elementCoordinates = p;
    }

    public void setLocation(int x, int y) {
        this.elementCoordinates = new Point(x, y);
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
