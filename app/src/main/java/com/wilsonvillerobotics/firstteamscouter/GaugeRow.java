package com.wilsonvillerobotics.firstteamscouter;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.TableRow;

/**
 * Created by SommervilleT on 2/13/2015.
 */
public class GaugeRow extends TableRow {
    private GameElement ge;
    private boolean rowActive;

    public GaugeRow(Context context) {
        super(context);
        this.ge = new GameElement();
        this.rowActive = false;
    }

    public GaugeRow(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.ge = new GameElement();
        this.rowActive = false;
    }

    public void setGameElement(GameElement ge) {
        this.ge = ge;
    }

}
