package com.wilsonvillerobotics.firstteamscouter;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.widget.ImageView;
import android.widget.TableLayout;
import android.widget.TableRow;

import java.util.ArrayList;

/**
 * Created by SommervilleT on 2/13/2015.
 */
public class GaugeLayout extends TableLayout {
    private int numRows;
    private Context context;
    //private ArrayList<GameElement> gameElements;
    //private ArrayList<ImageView> imageViews;
    private ArrayList<GaugeRow> gaugeRows;
    //private ArrayList<Integer>   activeRowIndices;

    public GaugeLayout(Context context) {
        super(context);
        this.context = context;
        //this.gameElements = new ArrayList<GameElement>();
        //this.imageViews = new ArrayList<ImageView>();
        this.gaugeRows = new ArrayList<GaugeRow>();
        //this.activeRowIndices = new ArrayList<Integer>();
        this.numRows = 0;
    }

    public GaugeLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.context = context;
        //this.gameElements = new ArrayList<GameElement>();
        //this.imageViews = new ArrayList<ImageView>();
        this.gaugeRows = new ArrayList<GaugeRow>();
        this.numRows = 0;
    }

    private void init() {
        this.numRows = this.getChildCount();

        for(int i = 0; i < numRows; i++) {
            GaugeRow tr = (GaugeRow)this.getChildAt(i);
            tr.setTag(String.valueOf(i));
            gaugeRows.add(i, tr);
            //imageViews.add(i, (ImageView)tr.findViewWithTag("rowTag"));
        }
    }

    public TableRow getRow(int index) {
        if(index >= numRows) return null;

        return gaugeRows.get(index);
    }

    public void addRow() {
        ImageView iv = new ImageView(context);
        //imageViews.add(this.numRows, iv);
        iv.setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50));
        iv.setTag("rowTag");

        TableLayout.LayoutParams lp = new TableLayout.LayoutParams(iv.getWidth(), iv.getHeight());
        iv.setLayoutParams(lp);

        TableRow tr = new TableRow(this.context);
        tr.setTag(String.valueOf(numRows++));
        tr.addView(iv);
        this.addView(tr);
    }

    public ImageView getImageView(int index) {
        if(index >= numRows) return null;

        //return imageViews.get(index);
        return null;
    }

    private void populateTableRows(int nRows) {
        for(int i = 0; i < nRows; i++) {
            this.addRow();
        }
    }

    private int getRowIndex(TableRow tr) {
        return (Integer)tr.getTag();
    }

    public int getActiveRowCount() {
        return 0; //return this.activeRowIndices.size();
    }

    public void activateRow(int rowIndex, GameElement.GameElementType get, GameElement.GameElementState ges) {
        if(rowIndex >= numRows) return;

        TableRow tr = this.gaugeRows.get(rowIndex);
        //ImageView iv = this.imageViews.get(rowIndex);
        //iv.setImageDrawable(this.getDrawableForElementTypeAndState(get, ges));
        //this.activeRowIndices.add(rowIndex);
    }

    public void deactivateRow(int rowIndex) {
        if(rowIndex >= numRows) return;

        TableRow tr = this.gaugeRows.get(rowIndex);
        //ImageView iv = this.imageViews.get(rowIndex);
        //iv.setImageDrawable(getResources().getDrawable(R.drawable.gray_tote_side_up_silhouette_106x50));
        //this.activeRowIndices.remove((Integer) rowIndex);
    }

    private Drawable getDrawableForElementTypeAndState(GameElement.GameElementType get, GameElement.GameElementState ges) {
        Drawable d = null;
        switch (get) {
            case ROBOT:
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
                    case UNKNOWN:
                        d = getResources().getDrawable(R.drawable.green_can_side_up_63x75);
                        break;
                }
                break;
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
                    case UNKNOWN:
                        d = getResources().getDrawable(R.drawable.gray_tote_side_up_106x50);
                        break;
                }
                break;
        }

        return d;
    }

    public ArrayList<TableRow> getInactiveRows() {
        ArrayList<TableRow> altr = new ArrayList<TableRow>();
        for(int i = 0; i < numRows; i++) {
            //if(!this.activeRowIndices.contains(i)) {
            //    altr.add(i, this.gaugeRows.get(i));
            //}
        }
        return altr;
    }

    public ArrayList<TableRow> getActiveRows() {
        ArrayList<TableRow> altr = new ArrayList<TableRow>();
        for(int i = 0; i < numRows; i++) {
            //if(this.activeRowIndices.contains(i)) {
            //    altr.add(i, this.gaugeRows.get(i));
            //}
        }
        return altr;
    }

    public ArrayList<Integer> getPackingList() {
        ArrayList<Integer> packList = new ArrayList<Integer>();

        ArrayList<TableRow> tr = this.getActiveRows();
        for(TableRow t : tr) {

        }
        return packList;
    }
}
