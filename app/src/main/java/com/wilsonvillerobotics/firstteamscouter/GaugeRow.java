package com.wilsonvillerobotics.firstteamscouter;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.widget.ImageView;
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
    private ImageView iv;
    private boolean rowActive;
    private int rowIndex;
    private RowState rowState;

    public GaugeRow(Context context) {
        super(context);
        this.ge = null;
        this.iv = null;
        this.rowIndex = -1;
        this.rowActive = false;
        this.rowState = RowState.INACTIVE;
    }

    public void setRowIndex(int ri) {
        this.rowIndex = ri;
    }

    public int getRowIndex() {
        return this.rowIndex;
    }

    public GaugeRow(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.ge = new GameElement();
        this.rowActive = false;
    }

    public void setGameElement(GameElement ge) {
        this.ge = ge;
    }

    public GameElement getGameElement() {
        return this.ge;
    }

    public void setImageView(ImageView iv) {
        this.iv = iv;
    }

    public ImageView getImageView() {
        return this.iv;
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
        this.iv.setImageDrawable(d);
        this.ge.setElementType(get);
        this.ge.setElementState(ges);
        this.setOnDragListener(null);
        this.rowState = RowState.HIGHLIGHTED;
    }

    public void unhighlight(GameElement.GameElementType get, GameElement.GameElementState ges, Drawable d) {
        this.iv.setImageDrawable(d);
        this.ge.setElementType(get);
        this.ge.setElementState(ges);
        this.setOnDragListener(null);
        this.rowState = RowState.INACTIVE;
    }

    public void activate(GameElement.GameElementType get, GameElement.GameElementState ges, Drawable d, OnTouchListener touchy) {
        this.iv.setImageDrawable(d);
        this.iv.setOnTouchListener(touchy);
        this.ge.setElementType(get);
        this.ge.setElementState(ges);
        this.setOnDragListener(null);
        //this.rowActive = true;
        this.rowState = RowState.ACTIVE;
    }

    public void deactivate(GameElement.GameElementType get, GameElement.GameElementState ges, OnDragListener dragger) {
        this.iv.setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50));
        iv.setOnTouchListener(null);
        this.ge.setElementType(GameElement.GameElementType.UNKNOWN);
        this.ge.setElementState(GameElement.GameElementState.UNKNOWN);
        this.setOnDragListener(dragger);
        //this.rowActive = false;
        this.rowState = RowState.INACTIVE;
    }
}
