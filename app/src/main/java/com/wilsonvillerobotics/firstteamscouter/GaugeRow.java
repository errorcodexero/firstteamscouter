package com.wilsonvillerobotics.firstteamscouter;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.widget.TableRow;

/**
 * Created by SommervilleT on 2/13/2015.
 */
public class GaugeRow extends TableRow {
    public enum RowState {
        ACTIVE ("Active"),
        INACTIVE ("Inactive"),
        HIGHLIGHTED ("Highlighted"),
        UNKNOWN ("Unknown");

        private String state;
        RowState(String state) {
            this.state = state;
        }

        public RowState getStateByString(String state) {
            for(RowState rs : RowState.values()) {
                if(rs.state.matches(state)) {
                    return rs;
                }
            }
            return UNKNOWN;
        }
    };

    private GameElement ge;
    private int rowIndex;
    private RowState rowState;
    private GameElement.GameElementType defaultGameElementType;
    private Drawable inactiveCan;
    private Drawable inactiveTote;

    public GaugeRow(Context context) {
        super(context);
        this.ge = null;
        this.rowIndex = -1;
        this.rowState = RowState.INACTIVE;
        this.defaultGameElementType = GameElement.GameElementType.UNKNOWN;
        this.inactiveCan = getResources().getDrawable(R.drawable.green_can_side_up_silhouette_106x50);
        this.inactiveTote = getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50);
    }

    public void setRowIndex(int ri) {
        this.rowIndex = ri;
    }

    public int getRowIndex() {
        return this.rowIndex;
    }

    public GaugeRow(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.ge = new GameElement(context);
        this.rowIndex = -1;
        this.rowState = RowState.INACTIVE;
        this.defaultGameElementType = GameElement.GameElementType.UNKNOWN;
        this.inactiveCan = getResources().getDrawable(R.drawable.green_can_side_up_silhouette_106x50);
        this.inactiveTote = getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50);
    }

    public void setGameElement(GameElement ge) {
        this.ge = ge;
    }

    public GameElement getGameElement() {
        return this.ge;
    }

    public void setDefaultGameElementType(GameElement.GameElementType get) {
        this.defaultGameElementType = get;
    }

    public GameElement.GameElementType getDefaultGameElementType() {
        return this.defaultGameElementType;
    }

    public boolean isInactive() {
        return this.rowState == RowState.INACTIVE;
    }

    public boolean isHighlighted() {
        return this.rowState == RowState.HIGHLIGHTED;
    }

    public boolean isActive() {
        return this.rowState == RowState.ACTIVE;
    }

    // TODO - Refactor the code below to get the drawable based on the element type and state, and the row state.
    //        Need to fix Telemode code first to send in the proper element type and state, right
    //        now it's only working with ImageView's from a LinearLayout, so type and state are lost.

    public void highlight(GameElement.GameElementType get, GameElement.GameElementState ges, Drawable d) {
        this.ge.setImageDrawable(d);
        this.ge.setElementType(get);
        this.ge.setElementState(ges);
        this.rowState = RowState.HIGHLIGHTED;
    }

    public void unhighlight(GameElement.GameElementType get, GameElement.GameElementState ges, Drawable d) {
        this.ge.setImageDrawable(d);
        this.ge.setElementType(get);
        this.ge.setElementState(ges);
        this.rowState = RowState.INACTIVE;
    }

    public void activate(GameElement.GameElementType get, GameElement.GameElementState ges, Drawable d, OnTouchListener touchy) {
        this.ge.setImageDrawable(d);
        this.ge.setOnTouchListener(touchy);
        this.ge.setElementType(get);
        this.ge.setElementState(ges);
        this.setOnDragListener(null);
        this.rowState = RowState.ACTIVE;
    }

    public void deactivate(GameElement.GameElementType get, GameElement.GameElementState ges, OnDragListener dragger) {
        if(this.defaultGameElementType == GameElement.GameElementType.CAN) {
            this.ge.setImageDrawable(this.inactiveCan);
        } else {
            this.ge.setImageDrawable(this.inactiveTote);
        }
        this.ge.setOnTouchListener(null);
        this.ge.setElementType(get);
        this.ge.setElementState(ges);
        this.setOnDragListener(dragger);
        this.rowState = RowState.INACTIVE;
    }
}
