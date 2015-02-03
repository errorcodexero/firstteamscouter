package com.wilsonvillerobotics.firstteamscouter;

import java.util.Hashtable;

import com.wilsonvillerobotics.firstteamscouter.TeamMatchData.STARTING_LOC;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.content.ClipData;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.DragEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.DragShadowBuilder;
import android.view.View.OnDragListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.Toast;
import android.widget.ToggleButton;
import android.support.v4.app.Fragment;

public class TeamMatchStartingPositionFragment extends Fragment implements OnClickListener {

	private TeamMatchData tmData;
	protected Long teamMatchID;
	public static String myTitle = "Starting Position";
	Hashtable<STARTING_LOC, ToggleButton> buttonHash;
	Hashtable<STARTING_LOC, String> helpHash;
	
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {

    	this.teamMatchID = getArguments() != null ? getArguments().getLong("tmID") : -1;
    	
        View rootView = inflater.inflate(R.layout.fragment_team_match_starting_position, container, false);
        
        int backgroundResource = R.drawable.starting_position_background_2015;
        if(this.tmData.tabletID.startsWith("Red")) {
        	backgroundResource = R.drawable.starting_position_background_2015; //starting_position_red_background;
        } else if(this.tmData.tabletID.startsWith("Blue")) {
        	backgroundResource = R.drawable.starting_position_background_2015; //starting_position_blue_background;
        }
        rootView.setBackgroundResource(backgroundResource);
        
        this.helpHash = new Hashtable<STARTING_LOC, String>();

        rootView.findViewById(R.id.imgRobot).setOnTouchListener(new MyTouchListener());
        rootView.findViewById(R.id.StartingPosition_LayoutRelative).setOnDragListener(new MyDragListener());

		return rootView;
	}

    private final class MyTouchListener implements View.OnTouchListener {
        @Override
        public boolean onTouch(View view, MotionEvent motionEvent) {
            if (motionEvent.getAction() == MotionEvent.ACTION_DOWN) {
                ClipData data = ClipData.newPlainText("", "");
                DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(view);
                view.startDrag(data, shadowBuilder, view, 0);
                //view.setVisibility(View.INVISIBLE);
                return true;
            } else if (motionEvent.getAction() == MotionEvent.ACTION_MOVE) {
                view.setVisibility(View.GONE);
                return true;
            } else if (motionEvent.getAction() == MotionEvent.ACTION_UP) {
                //view.setVisibility(View.VISIBLE);
                return true;
            } else {
                return false;
            }
        }
    }
    
    private void updateToggleButtonStates() {
    	FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::updateToggleButtons\n");
    	if(this.tmData != null) {
    		STARTING_LOC startPos = this.tmData.getStartingPosition();
    		FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::updateToggleButtons : SL: " + startPos.toString() + "\n");
    		if(startPos != STARTING_LOC.FIELD_NOT_SET) {
    			this.buttonHash.get(startPos).setChecked(true);
    		}
    	}
    }
    
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
    	super.onActivityCreated(savedInstanceState);
/*    	final View myView = getView();
    	
        
    	myView.getViewTreeObserver().addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
            @Override
            public void onGlobalLayout() {
                fragmentWidth = getView().getWidth();
                fragmentHeight = getView().getHeight();
                if(fragmentWidth > 0)
                {
                    // Width greater than 0, layout should be done.
                    // Set the textviews and remove the listener.
                	txtWidth.setText(String.valueOf(fragmentWidth));
                	txtHeight.setText(String.valueOf(fragmentHeight));
                	
                    getView().getViewTreeObserver().removeGlobalOnLayoutListener(this);
                }
            }
          });*/
    }
    
    static TeamMatchStartingPositionFragment newInstance(Long teamMatchID) {
    	TeamMatchStartingPositionFragment f = new TeamMatchStartingPositionFragment();

        // Supply num input as an argument.
        Bundle args = new Bundle();
        args.putLong("tmID", teamMatchID);
        f.setArguments(args);

        return f;
    }
    
    public void setTeamMatchData(TeamMatchData tmD) {
    	this.tmData = tmD;
    }
    
    @Override
	public void onClick(View v) {

    }
    
	private void resetToggleButtonsExcept(STARTING_LOC loc) {
		ToggleButton tempButton;

		for(STARTING_LOC l : buttonHash.keySet()) {
			if(l != loc) {
				FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::resetToggleButtonsExcept : Resetting: " + l.toString() + "\n");
				tempButton = (ToggleButton) buttonHash.get(l);
				tempButton.setChecked(false);
				tempButton.setPressed(false);
			}
		}
	}
	
	private void setStartingLoc(STARTING_LOC sl) {
		if(this.tmData != null) {
			FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::setStartingLoc : Setting SL in tmData: " + sl.toString() + "\n");
			this.tmData.setStartingLoc(sl);
		}
	}
	
	private void setOrResetStartingLoc(STARTING_LOC sl, boolean isChecked) {
		if(isChecked) {
			FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::setOrResetStartingLoc : Setting SL: " + sl.toString() + "\n");
			this.setStartingLoc(sl);
		} else {
			FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::setOrResetStartingLoc : Resetting SL: " + sl.toString() + "\n");
			this.setStartingLoc(STARTING_LOC.FIELD_NOT_SET);
		}
		this.tmData.setSavedDataState(true, "setOrResetStartingLoc");
	}

	private void positionButtonClick(STARTING_LOC sl) {
		ToggleButton tb = buttonHash.get(sl);
		EnterTeamMatchDataActivity act = ((EnterTeamMatchDataActivity)this.getActivity());
		
		if(act.helpActive) {
			tb.setChecked(!tb.isChecked());
			act.disableHelp();
			String message = this.helpHash.get(sl);
			Toast.makeText(getActivity(), message, Toast.LENGTH_LONG).show();
		} else {
			this.resetToggleButtonsExcept(sl);
			this.setOrResetStartingLoc(sl, tb.isChecked());
		}
	}

    class MyDragListener implements OnDragListener {
        Drawable enterShape = getResources().getDrawable(R.drawable.blue_metallic_outline_toggle_on);
        Drawable normalShape = getResources().getDrawable(R.drawable.blue_metallic_outline_toggle_off);

        @Override
        public boolean onDrag(View v, DragEvent event) {
            int action = event.getAction();
            switch (action) {
                case DragEvent.ACTION_DRAG_STARTED:
                    // do nothing
                    break;
                case DragEvent.ACTION_DRAG_ENTERED:
                    //v.setBackgroundDrawable(enterShape);
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    //v.setBackgroundDrawable(normalShape);
                    break;
                case DragEvent.ACTION_DROP:
                    // Dropped, reassign View to ViewGroup
                    //FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::DragEvent::ACTION_DROP\n");
                    View view = (View) event.getLocalState();

                    //String toastText = "onDrag Event X: " + event.getX() + " Y: " + event.getY();
                    //Toast.makeText(getActivity(), toastText, Toast.LENGTH_LONG).show();

                    RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(
                            RelativeLayout.LayoutParams.WRAP_CONTENT,
                            RelativeLayout.LayoutParams.WRAP_CONTENT
                    );
                    params.alignWithParent = true;
                    params.addRule(RelativeLayout.ALIGN_PARENT_TOP);
                    params.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
                    params.setMargins((int)event.getX() - ((int)((View) event.getLocalState()).getWidth() / 2), (int)event.getY() - ((int)((View) event.getLocalState()).getHeight() / 2), 0, 0);
                    view.setLayoutParams(params);
                    view.setVisibility(View.VISIBLE);

                    //ViewGroup owner = (ViewGroup) view.getParent();
                    //owner.removeView(view);
                    //LinearLayout container = (LinearLayout) v;
                    //container.addView(view);
                    //view.setVisibility(View.VISIBLE);
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    v.setVisibility(View.VISIBLE);
                    break;
                default:
                    break;
            }
            return true;
        }
    }
}
