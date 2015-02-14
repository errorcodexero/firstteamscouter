package com.wilsonvillerobotics.firstteamscouter;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.widget.ImageView;
import android.widget.TableLayout;
import android.widget.TableRow;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Created by SommervilleT on 2/13/2015.
 */
public class GaugeLayout extends TableLayout {
    private int numRows;
    private Context context;
    private ArrayList<GaugeRow> gaugeRows;

    public GaugeLayout(Context context) {
        super(context);
        this.context = context;
        this.gaugeRows = new ArrayList<GaugeRow>();
        this.numRows = 0;
        this.init();
    }

    public GaugeLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.context = context;
        this.gaugeRows = new ArrayList<GaugeRow>();
        this.numRows = 0;
        this.init();
    }

    private void init() {
        this.init(null);
    }

    public void init(OnDragListener odl) {
        this.numRows = this.getChildCount();
        for(int i = 0; i < numRows; i++) {
            GaugeRow gr = (GaugeRow)this.getChildAt(i);
            gr.setRowIndex(numRows - i - 1);
            gr.setTag(String.valueOf(i));

            gr.setOnDragListener(odl);

            ImageView iv = (ImageView) gr.findViewWithTag("rowImageView");
            GameElement ge = new GameElement();
            ge.setImageView(iv);
            ge.setVisibility(true);
            ge.setElementType(GameElement.GameElementType.UNKNOWN);
            ge.setElementState(GameElement.GameElementState.UNKNOWN);

            gr.setImageView(iv);
            gr.setGameElement(ge);
            gaugeRows.add(i, gr);
        }
        Collections.reverse(gaugeRows);
    }

    public TableRow getRowByIndex(int index) {
        if(index >= numRows) return null;
        return gaugeRows.get(index);
    }

    public void addRow() {
        GameElement ge = new GameElement();

        ImageView iv = new ImageView(context);
        iv.setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50));
        iv.setTag("rowImageView");

        TableLayout.LayoutParams lp = new TableLayout.LayoutParams(iv.getWidth(), iv.getHeight());
        iv.setLayoutParams(lp);
        ge.setImageView(iv);

        GaugeRow tr = new GaugeRow(this.context);
        tr.setImageView(iv);
        tr.setTag(String.valueOf(numRows++));
        tr.addView(iv);
        this.addView(tr);
    }

    public ImageView getImageView(int index) {
        if(index >= numRows) return null;
        return this.gaugeRows.get(index).getImageView();
    }

    private void populateTableRows(int nRows) {
        for(int i = 0; i < nRows; i++) {
            this.addRow();
        }
    }

    private int getRowIndex(GaugeRow tr) {
        if(tr == null) return -1;
        return tr.getRowIndex();
    }

    public int getActiveRowCount() {
        int activeRows = 0;
        for(GaugeRow gr : gaugeRows) {
            if(gr.isActive()) {
                activeRows++;
            }
        }
        return activeRows;
    }

    public void activateRow(int rowIndex, GameElement.GameElementType get, GameElement.GameElementState ges) {
        if(rowIndex >= numRows) return;
        this.gaugeRows.get(rowIndex).activate(get, ges, getDrawableForElementTypeAndState(get, ges));
    }

    public void deactivateRow(int rowIndex) {
        if(rowIndex >= numRows) return;
        this.gaugeRows.get(rowIndex).deactivate();
    }

    private Drawable getDrawableForElementTypeAndState(GameElement.GameElementType get, GameElement.GameElementState ges) {
        Drawable d = null;
        switch (get) {
            case ROBOT:
                d = getResources().getDrawable(R.drawable.robot_50x50);
                break;
            case GRAY_TOTE:
                switch (ges){
                    case UPRIGHT:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_up_106x50);
                        break;
                    case ONSIDE:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_up_106x50);
                        break;
                    case UPSIDEDOWN:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_down_106x50);
                        break;
                    case ONEND:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_up_106x50);
                        break;
                    default:
                    case UNKNOWN:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_up_106x50);
                        break;
                }
                break;
            case YELLOW_TOTE:
                switch (ges){
                    case UPRIGHT:
                        d = getResources().getDrawable(R.drawable.yellow_tote_side_up_106x50);
                        break;
                    case ONSIDE:
                        d = getResources().getDrawable(R.drawable.yellow_tote_side_up_106x50);
                        break;
                    case UPSIDEDOWN:
                        d = getResources().getDrawable(R.drawable.yellow_tote_side_up_106x50);
                        break;
                    case ONEND:
                        d = getResources().getDrawable(R.drawable.yellow_tote_side_up_106x50);
                        break;
                    default:
                    case UNKNOWN:
                        d = getResources().getDrawable(R.drawable.yellow_tote_side_up_106x50);
                        break;
                }
                break;
            case CAN:
                switch (ges){
                    case UPRIGHT:
                        d = getResources().getDrawable(R.drawable.green_can_side_up_63x75);
                        break;
                    case ONSIDE:
                        d = getResources().getDrawable(R.drawable.green_can_side_side_75x63);
                        break;
                    case UPSIDEDOWN:
                        d = getResources().getDrawable(R.drawable.green_can_side_down_63x75);
                        break;
                    case ONEND:
                        d = getResources().getDrawable(R.drawable.green_can_side_up_63x75);
                        break;
                    default:
                    case UNKNOWN:
                        d = getResources().getDrawable(R.drawable.green_can_side_up_63x75);
                        break;
                }
                break;
            case TRASH:
                switch (ges){
                    case UPRIGHT:
                        d = getResources().getDrawable(R.drawable.green_can_side_up_63x75);
                        break;
                    case ONSIDE:
                        d = getResources().getDrawable(R.drawable.green_can_side_side_75x63);
                        break;
                    case UPSIDEDOWN:
                        d = getResources().getDrawable(R.drawable.green_can_side_down_63x75);
                        break;
                    case ONEND:
                        d = getResources().getDrawable(R.drawable.green_can_side_up_63x75);
                        break;
                    default:
                    case UNKNOWN:
                        d = getResources().getDrawable(R.drawable.green_can_side_up_63x75);
                        break;
                }
                break;
            default:
            case UNKNOWN:
                switch (ges){
                    case UPRIGHT:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_up_106x50);
                        break;
                    case ONSIDE:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_up_106x50);
                        break;
                    case UPSIDEDOWN:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_down_106x50);
                        break;
                    case ONEND:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_up_106x50);
                        break;
                    default:
                    case UNKNOWN:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_up_106x50);
                        break;
                }
                break;
        }

        return d;
    }

    public ArrayList<GaugeRow> getInactiveRows() {
        ArrayList<GaugeRow> altr = new ArrayList<GaugeRow>();
        for(GaugeRow gr : gaugeRows) {
            if(!gr.isActive()) {
                altr.add(gr);
            }
        }
        return altr;
    }

    public ArrayList<GaugeRow> getActiveRows() {
        ArrayList<GaugeRow> altr = new ArrayList<GaugeRow>();
        for(GaugeRow gr : gaugeRows) {
            if(gr.isActive()) {
                altr.add(gr);
            }
        }
        return altr;
    }

    public ArrayList<Drawable> getPackingList() {
        ArrayList<Drawable> packList = new ArrayList<Drawable>();

        ArrayList<GaugeRow> tr = this.getActiveRows();
        for(GaugeRow t : tr) {
            packList.add(t.getImageView().getDrawable());
            t.deactivate();
        }
        return packList;
    }

    public ArrayList<GaugeRow> getInActiveRowsAbove(GaugeRow gr) {
        ArrayList<GaugeRow> altr = new ArrayList<GaugeRow>();
        for(int i = 0; i < gr.getRowIndex(); i++) {
            if(this.gaugeRows.get(i).isActive()) {
                altr.add(gr);
            }
        }
        return altr;
    }

    public ArrayList<GaugeRow> getInActiveRowsBelow(GaugeRow gr) {
        ArrayList<GaugeRow> altr = new ArrayList<GaugeRow>();
        for(int i = gr.getRowIndex(); i > 0 ; i--) {
            if(!this.gaugeRows.get(i).isActive()) {
                altr.add(gr);
            }
        }
        return altr;
    }

    public int getInActiveRowsAboveCount(GaugeRow gr) {
        int numRows = 0;
        for(int i = 0; i < gr.getRowIndex(); i++) {
            if(this.gaugeRows.get(i).isActive()) {
                numRows++;
            }
        }
        return numRows;
    }

    public int getInActiveRowsBelowCount(GaugeRow gr) {
        int numRows = 0;
        for(int i = gr.getRowIndex(); i > 0 ; i--) {
            if(!this.gaugeRows.get(i).isActive()) {
                numRows++;
            }
        }
        return numRows;
    }
}