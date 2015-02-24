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
    private GameElement ge;
    private ImageView iv;
    private boolean rowActive;
    private int rowIndex;

    public GaugeRow(Context context) {
        super(context);
        this.ge = null;
        this.iv = null;
        this.rowIndex = -1;
        this.rowActive = false;
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

    public boolean isActive() {
        return this.rowActive;
    }

    public void activate(GameElement.GameElementType get, GameElement.GameElementState ges, Drawable d, OnTouchListener touchy) {
        this.iv.setImageDrawable(d);
        this.iv.setOnTouchListener(touchy);
        this.ge.setElementType(get);
        this.ge.setElementState(ges);
        this.setOnDragListener(null);
        this.rowActive = true;
    }

    public void deactivate(OnDragListener dragger) {
        this.iv.setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50));
        iv.setOnTouchListener(null);
        this.ge.setElementType(GameElement.GameElementType.UNKNOWN);
        this.ge.setElementState(GameElement.GameElementState.UNKNOWN);
        this.setOnDragListener(dragger);
        this.rowActive = false;
    }
}
