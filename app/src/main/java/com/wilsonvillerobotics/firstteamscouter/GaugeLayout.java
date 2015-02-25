package com.wilsonvillerobotics.firstteamscouter;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.widget.ImageView;
import android.widget.LinearLayout;
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

    public enum GaugeType {
        ROBOT ("Robot Gauge"),
        FLOOR ("Floor Gauge"),
        PLATFORM ("Platform Gauge"),
        STEP ("Step Gauge"),
        UNKNOWN ("Unknown Gauge");

        String type;
        GaugeType(String type) {
            this.type = type;
        }

        public GaugeType getgaugeByString(String type) {
            for(GaugeType gt : GaugeType.values()) {
                if(gt.type.matches(type)) return gt;
            }
            return UNKNOWN;
        }

        public String getGaugeTypeString() {
            return this.type;
        }
    }

    private String gaugeName;
    private GaugeType gaugeType;
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
        this.gaugeType = GaugeType.UNKNOWN;
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

    public void setGaugeType(GaugeType gaugeType) {
        this.gaugeType = gaugeType;
    }

    public  GaugeType getGaugeType() {
        return this.gaugeType;
    }

    public void setGaugeName(String name) {
        this.gaugeName = name;
    }

    public String getGaugeName() {
        return this.gaugeName;
    }

    public GaugeRow getRowAtIndex(int index) {
        if(index >= numRows || index < 0) return null;
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

    public void activateRow(int rowIndex, GameElement.GameElementType get, GameElement.GameElementState ges, OnTouchListener touchy) {
        if(rowIndex >= numRows) return;
        this.gaugeRows.get(rowIndex).activate(get, ges, getDrawableForElementTypeAndState(get, ges), touchy);
    }

    public void deactivateRow(int rowIndex, OnDragListener dragger) {
        if(rowIndex >= numRows) return;
        this.gaugeRows.get(rowIndex).deactivate(GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, dragger);
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

    public void highlightRows(GaugeRow gaugeRow, int numRows) {
        if(gaugeRow.isInactive()) {
            int rowsModTwo = numRows % 2;
            int halfNumRows = numRows / 2;
            int curIndex = gaugeRow.getRowIndex();
            int lowerBounds = curIndex - halfNumRows + (1 - rowsModTwo);
            int upperBounds = curIndex + halfNumRows;

            if(lowerBounds >= 0 && upperBounds < this.numRows) {
                boolean foundAllInactiveRows = true;
                ArrayList<GaugeRow> rowsToHighlight = new ArrayList<GaugeRow>();
                for(int i = lowerBounds; i <= upperBounds; i++) {
                    GaugeRow gr = this.getRowAtIndex(i);
                    foundAllInactiveRows &= gr.isInactive();
                    rowsToHighlight.add(gr);
                }

                if (foundAllInactiveRows) {
                    for (GaugeRow gr : rowsToHighlight) {
                        Drawable d = getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_light_106x50);
                        gr.highlight(GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, d);
                        //gr.getImageView().setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_light_106x50));
                    }
                }
            }
        }
    }

    public void unHighlightRows(GaugeRow gaugeRow, int numRows) {
        if (gaugeRow.isHighlighted()) {
            int rowsModTwo = numRows % 2;
            int halfNumRows = numRows / 2;
            int curIndex = gaugeRow.getRowIndex();
            int lowerBounds = curIndex - halfNumRows + (1 - rowsModTwo);
            int upperBounds = curIndex + halfNumRows;

            if(lowerBounds >= 0 && upperBounds < this.numRows) {
                boolean foundOnlyHighlightedRows = true;
                ArrayList<GaugeRow> rowsToUnhighlight = new ArrayList<GaugeRow>();
                for(int i = lowerBounds; i <= upperBounds; i++) {
                    GaugeRow gr = this.getRowAtIndex(i);
                    foundOnlyHighlightedRows &= gr.isHighlighted();
                    rowsToUnhighlight.add(gr);
                }

                if (foundOnlyHighlightedRows) {
                    for (GaugeRow gr : rowsToUnhighlight) {
                        Drawable d = getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50);
                        gr.unhighlight(GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, d);
                        //gr.getImageView().setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50));
                    }
                }
            }
        }
    }

    public void activateRows(GaugeRow gaugeRow, LinearLayout llElements, OnTouchListener touchy) {
        int numRows = llElements.getChildCount();
        if(!gaugeRow.isActive()) {
            int rowsModTwo = numRows % 2;
            int halfNumRows = numRows / 2;
            int curIndex = gaugeRow.getRowIndex();
            int lowerBounds = curIndex - halfNumRows + (1 - rowsModTwo);
            int upperBounds = curIndex + halfNumRows;

            if(lowerBounds >= 0 && upperBounds < this.numRows) {
                boolean foundAllInactiveRows = true;
                ArrayList<GaugeRow> rowsToActivate = new ArrayList<GaugeRow>();
                for(int i = lowerBounds; i <= upperBounds; i++) {
                    GaugeRow gr = this.getRowAtIndex(i);
                    foundAllInactiveRows &= !gr.isActive();
                    rowsToActivate.add(gr);
                }

                if (foundAllInactiveRows) {
                    Collections.reverse(rowsToActivate);
                    for(int i = 0; i < rowsToActivate.size() && i < numRows; i++) {
                        GaugeRow gr = rowsToActivate.get(i);
                        ImageView iv = (ImageView)llElements.getChildAt(i);
                        //gr.getImageView().setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_light_106x50));
                        GameElement.GameElementType get = GameElement.GameElementType.getTypeByString("");
                        GameElement.GameElementState ges = GameElement.GameElementState.getStateByString("");
                        //Drawable d = this.getDrawableForElementTypeAndState(gr.getGameElement().getElementType(), gr.getGameElement().getElementState());
                        //gr.getImageView().setImageDrawable(iv.getDrawable());
                        gr.activate(get, ges, iv.getDrawable(), touchy);
                    }
                }
            }
        }
    }

    public void deactivateRows(GaugeRow gaugeRow, LinearLayout llElements, OnDragListener draggy) {
        int numRows = llElements.getChildCount();
        if(gaugeRow.isInactive()) {
            int rowsModTwo = numRows % 2;
            int halfNumRows = numRows / 2;
            int curIndex = gaugeRow.getRowIndex();
            int lowerBounds = curIndex - halfNumRows + (1 - rowsModTwo);
            int upperBounds = curIndex + halfNumRows;

            if(lowerBounds >= 0 && upperBounds < this.numRows) {
                boolean foundAllActiveRows = true;
                ArrayList<GaugeRow> rowsToDeactivate = new ArrayList<GaugeRow>();
                for(int i = lowerBounds; i <= upperBounds; i++) {
                    GaugeRow gr = this.getRowAtIndex(i);
                    foundAllActiveRows &= gr.isActive();
                    rowsToDeactivate.add(gr);
                }
                if (foundAllActiveRows) {
                    for(int i = 0; i < rowsToDeactivate.size() && i < numRows; i++) {
                        GaugeRow gr = rowsToDeactivate.get(i);
                        ImageView iv = (ImageView)llElements.getChildAt(i);
                        //gr.getImageView().setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_light_106x50));
                        GameElement.GameElementType get = GameElement.GameElementType.getTypeByString("");
                        GameElement.GameElementState ges = GameElement.GameElementState.getStateByString("");
                        //Drawable d = this.getDrawableForElementTypeAndState(gr.getGameElement().getElementType(), gr.getGameElement().getElementState());
                        //gr.getImageView().setImageDrawable(iv.getDrawable());
                        gr.deactivate(get, ges, draggy);
                    }
                }
            }
        }
    }

    public void deactivateAllRows(OnDragListener dragger) {
        for(GaugeRow gr : this.gaugeRows) {
            gr.deactivate(GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, dragger);
        }
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

    public ArrayList<Drawable> getPackingList(OnDragListener dragger) {
        ArrayList<Drawable> packList = new ArrayList<Drawable>();

        ArrayList<GaugeRow> tr = this.getActiveRows();
        for(GaugeRow t : tr) {
            packList.add(t.getImageView().getDrawable());
            t.deactivate(GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, dragger);
        }
        return packList;
    }

    public ArrayList<GaugeRow> getInActiveRowsAbove(GaugeRow gr) {
        ArrayList<GaugeRow> altr = new ArrayList<GaugeRow>();
        for(int i = gr.getRowIndex(); i < this.numRows; i++) {
            GaugeRow curGr = this.gaugeRows.get(i);
            if(!curGr.isActive()) {
                altr.add(curGr);
            }
        }
        return altr;
    }

    public ArrayList<GaugeRow> getActiveRowsAbove(GaugeRow gr) {
        ArrayList<GaugeRow> altr = new ArrayList<GaugeRow>();
        for(int i = gr.getRowIndex(); i < this.numRows; i++) {
            GaugeRow curGr = this.gaugeRows.get(i);
            if(curGr.isActive()) {
                altr.add(curGr);
            } else {
                break;
            }
        }
        return altr;
    }

    public ArrayList<GaugeRow> getInActiveRowsBelow(GaugeRow gr) {
        ArrayList<GaugeRow> altr = new ArrayList<GaugeRow>();
        for(int i = gr.getRowIndex(); i >= 0 ; i--) {
            GaugeRow curGr = this.gaugeRows.get(i);
            if(!curGr.isActive()) {
                altr.add(curGr);
            } else {
                break;
            }
        }
        return altr;
    }

    public int getInActiveRowsAboveCount(GaugeRow gr) {
        int numRows = 0;
        for(int i = gr.getRowIndex(); i < this.numRows; i++) {
            if(!this.gaugeRows.get(i).isActive()) {
                numRows++;
            } else {
                break;
            }
        }
        return numRows;
    }

    public int getInActiveRowsBelowCount(GaugeRow gr) {
        int numRows = 0;
        for(int i = gr.getRowIndex(); i >= 0 ; i--) {
            if(!this.gaugeRows.get(i).isActive()) {
                numRows++;
            } else {
                break;
            }
        }
        return numRows;
    }
}