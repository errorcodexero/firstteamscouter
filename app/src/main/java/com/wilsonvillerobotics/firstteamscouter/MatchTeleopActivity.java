package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import java.lang.reflect.Array;
import java.util.ArrayList;

/**
 * Created by Luke on 3/19/2016.
 */
public class MatchTeleopActivity extends Activity implements View.OnClickListener{


    enum ArrayIndeces{
        HIGH_GOAL_SCORE (0),
        HIGH_GOAL_MISS (1),
        LOW_GOAL_SCORE (2),
        LOW_GOAL_MISS (3),
        LOW_BAR_SCORE (4),
        LOW_BAR_UNSCORE (5),
        DEFENSE_A_SCORE(6),
        DEFENSE_A_UNSCORE (7),
        DEFENSE_B_SCORE (8),
        DEFENSE_B_UNSCORE (9),
        DEFENSE_C_SCORE (10),
        DEFENSE_C_UNSCORE (11),
        DEFENSE_D_SCORE (12),
        DEFENSE_D_UNSCORE (13);

        private final int index;
        ArrayIndeces(int i) {
            this.index = i;
        }
        Integer getIndex(){
            return this.index;
        }
    }
    ArrayList<Button>MinusButtons;
    ArrayList<Button>PlusButtons;
    ArrayList<TextView>ValueLabels;
    ArrayList<Integer>Values;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_match_teleop);

        //Minus Button Instantiation
        MinusButtons = new ArrayList<Button>(14);
        MinusButtons.add((Button)findViewById(R.id.btnHighGoalScoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnHighGoalMissMinus));
        MinusButtons.add((Button)findViewById(R.id.btnLowGoalScoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnLowGoalMissMinus));
        MinusButtons.add((Button)findViewById(R.id.btnLowBarScoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnLowBarUnscoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnDefenseAScoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnDefenseAUnscoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnDefenseBScoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnDefenseBUnscoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnDefenseCScoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnDefenseCUnscoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnDefenseDScoreMinus));
        MinusButtons.add((Button)findViewById(R.id.btnDefenseDUnscoreMinus));
        for(int i = 0; i < MinusButtons.size(); i++) {
            MinusButtons.get(i).setOnClickListener(this);
        }

        //Plus Button Instantiation
        PlusButtons = new ArrayList<Button>(14);
        PlusButtons.add((Button)findViewById(R.id.btnHighGoalScorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnHighGoalMissPlus));
        PlusButtons.add((Button)findViewById(R.id.btnLowGoalScorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnLowGoalMissPlus));
        PlusButtons.add((Button)findViewById(R.id.btnLowBarScorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnLowBarUnscorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnDefenseAScorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnDefenseAUnscorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnDefenseBScorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnDefenseBUnscorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnDefenseCScorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnDefenseCUnscorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnDefenseDScorePlus));
        PlusButtons.add((Button)findViewById(R.id.btnDefenseDUnscorePlus));
        for(int i = 0; i < PlusButtons.size(); i++) {
            PlusButtons.get(i).setOnClickListener(this);
        }

        //Label Instantiation
        ValueLabels = new ArrayList<TextView>(14);
        ValueLabels.add((TextView)findViewById(R.id.lblHighGoalScore));
        ValueLabels.add((TextView)findViewById(R.id.lblHighGoalMiss));
        ValueLabels.add((TextView)findViewById(R.id.lblLowGoalScore));
        ValueLabels.add((TextView)findViewById(R.id.lblLowGoalMiss));
        ValueLabels.add((TextView)findViewById(R.id.lblLowBarScore));
        ValueLabels.add((TextView)findViewById(R.id.lblLowBarUnscore));
        ValueLabels.add((TextView)findViewById(R.id.lblDefenseAScore));
        ValueLabels.add((TextView)findViewById(R.id.lblDefenseAUnscore));
        ValueLabels.add((TextView)findViewById(R.id.lblDefenseBScore));
        ValueLabels.add((TextView)findViewById(R.id.lblDefenseBUnscore));
        ValueLabels.add((TextView)findViewById(R.id.lblDefenseCScore));
        ValueLabels.add((TextView)findViewById(R.id.lblDefenseCUnscore));
        ValueLabels.add((TextView)findViewById(R.id.lblDefenseDScore));
        ValueLabels.add((TextView)findViewById(R.id.lblDefenseDUnscore));


        Values = new ArrayList<Integer>(ValueLabels.size());
        for(int i = 0; i < ValueLabels.size(); i++){
            Values.add(0);
        }

        refreshValueLabels();
    }

    public void refreshValueLabels(){
        if(ValueLabels.size() != Values.size()){
            return;
        }
        for(int i = 0; i < ValueLabels.size(); i++){
            ValueLabels.get(i).setText(Values.get(i).toString());
        }
    }


    //Helper function to set value and cap at 0 GOES HERE
    public void setValue(Integer index, Boolean add){
        if(add){
            Values.set(index, Values.get(index) + 1);
        }
        else{
            int val = (Values.get(index) > 0) ? Values.get(index) - 1 : 0;
            Values.set(index, val);
        }


    }
    @Override
    public void onClick(View view) {
        switch(view.getId()){
            case R.id.btnHighGoalScoreMinus:
                setValue(ArrayIndeces.HIGH_GOAL_SCORE.getIndex(), false);
                break;
            case R.id.btnHighGoalMissMinus:
                Values.set(ArrayIndeces.HIGH_GOAL_MISS.getIndex(), Values.get(ArrayIndeces.HIGH_GOAL_MISS.getIndex()) - 1);
                break;
            case R.id.btnLowGoalScoreMinus:
                Values.set(ArrayIndeces.LOW_GOAL_SCORE.getIndex(), Values.get(ArrayIndeces.LOW_GOAL_SCORE.getIndex()) - 1);
                break;
            case R.id.btnLowGoalMissMinus:
                Values.set(ArrayIndeces.LOW_GOAL_MISS.getIndex(), Values.get(ArrayIndeces.LOW_GOAL_MISS.getIndex()) - 1);
                break;
            case R.id.btnLowBarScoreMinus:
                Values.set(ArrayIndeces.LOW_BAR_SCORE.getIndex(), Values.get(ArrayIndeces.LOW_BAR_SCORE.getIndex()) - 1);
                break;
            case R.id.btnLowBarUnscoreMinus:
                Values.set(ArrayIndeces.LOW_BAR_UNSCORE.getIndex(), Values.get(ArrayIndeces.LOW_BAR_UNSCORE.getIndex()) - 1);
                break;
            case R.id.btnDefenseAScoreMinus:
                Values.set(ArrayIndeces.DEFENSE_A_SCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_A_SCORE.getIndex()) - 1);
                break;
            case R.id.btnDefenseAUnscoreMinus:
                Values.set(ArrayIndeces.DEFENSE_A_UNSCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_A_UNSCORE.getIndex()) - 1);
                break;
            case R.id.btnDefenseBScoreMinus:
                Values.set(ArrayIndeces.DEFENSE_B_SCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_B_SCORE.getIndex()) - 1);
                break;
            case R.id.btnDefenseBUnscoreMinus:
                Values.set(ArrayIndeces.DEFENSE_B_UNSCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_B_UNSCORE.getIndex()) - 1);
                break;
            case R.id.btnDefenseCScoreMinus:
                Values.set(ArrayIndeces.DEFENSE_C_SCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_C_SCORE.getIndex()) - 1);
                break;
            case R.id.btnDefenseCUnscoreMinus:
                Values.set(ArrayIndeces.DEFENSE_C_UNSCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_C_UNSCORE.getIndex()) - 1);
                break;
            case R.id.btnDefenseDScoreMinus:
                Values.set(ArrayIndeces.DEFENSE_D_SCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_D_SCORE.getIndex()) - 1);
                break;
            case R.id.btnDefenseDUnscoreMinus:
                Values.set(ArrayIndeces.DEFENSE_D_UNSCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_D_UNSCORE.getIndex()) - 1);
                break;

            case R.id.btnHighGoalScorePlus:
                Values.set(ArrayIndeces.HIGH_GOAL_SCORE.getIndex(), Values.get(ArrayIndeces.HIGH_GOAL_SCORE.getIndex()) + 1);
                break;
            case R.id.btnHighGoalMissPlus:
                Values.set(ArrayIndeces.HIGH_GOAL_MISS.getIndex(), Values.get(ArrayIndeces.HIGH_GOAL_MISS.getIndex()) + 1);
                break;
            case R.id.btnLowGoalScorePlus:
                Values.set(ArrayIndeces.LOW_GOAL_SCORE.getIndex(), Values.get(ArrayIndeces.LOW_GOAL_SCORE.getIndex()) + 1);
                break;
            case R.id.btnLowGoalMissPlus:
                Values.set(ArrayIndeces.LOW_GOAL_MISS.getIndex(), Values.get(ArrayIndeces.LOW_GOAL_MISS.getIndex()) + 1);
                break;
            case R.id.btnLowBarScorePlus:
                Values.set(ArrayIndeces.LOW_BAR_SCORE.getIndex(), Values.get(ArrayIndeces.LOW_BAR_SCORE.getIndex()) + 1);
                break;
            case R.id.btnLowBarUnscorePlus:
                Values.set(ArrayIndeces.LOW_BAR_UNSCORE.getIndex(), Values.get(ArrayIndeces.LOW_BAR_UNSCORE.getIndex()) + 1);
                break;
            case R.id.btnDefenseAScorePlus:
                Values.set(ArrayIndeces.DEFENSE_A_SCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_A_SCORE.getIndex()) + 1);
                break;
            case R.id.btnDefenseAUnscorePlus:
                Values.set(ArrayIndeces.DEFENSE_A_UNSCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_A_UNSCORE.getIndex()) + 1);
                break;
            case R.id.btnDefenseBScorePlus:
                Values.set(ArrayIndeces.DEFENSE_B_SCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_B_SCORE.getIndex()) + 1);
                break;
            case R.id.btnDefenseBUnscorePlus:
                Values.set(ArrayIndeces.DEFENSE_B_UNSCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_B_UNSCORE.getIndex()) + 1);
                break;
            case R.id.btnDefenseCScorePlus:
                Values.set(ArrayIndeces.DEFENSE_C_SCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_C_SCORE.getIndex()) + 1);
                break;
            case R.id.btnDefenseCUnscorePlus:
                Values.set(ArrayIndeces.DEFENSE_C_UNSCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_C_UNSCORE.getIndex()) + 1);
                break;
            case R.id.btnDefenseDScorePlus:
                Values.set(ArrayIndeces.DEFENSE_D_SCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_D_SCORE.getIndex()) + 1);
                break;
            case R.id.btnDefenseDUnscorePlus:
                Values.set(ArrayIndeces.DEFENSE_D_UNSCORE.getIndex(), Values.get(ArrayIndeces.DEFENSE_D_UNSCORE.getIndex()) + 1);
                break;

        }
        refreshValueLabels();
    }
}
