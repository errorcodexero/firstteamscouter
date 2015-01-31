package com.wilsonvillerobotics.firstteamscouter;

import java.util.Hashtable;

import com.wilsonvillerobotics.firstteamscouter.TeamMatchData.STARTING_LOC;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.content.ClipData;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.DragEvent;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.RelativeLayout;
import android.widget.Toast;
import android.widget.ToggleButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.Switch;
import android.widget.TextView;
import android.support.v4.app.Fragment;

public class TeamMatchAutoModeFragment extends Fragment implements OnClickListener{
	public static String myTitle;
	protected Long teamMatchID;
	private TeamMatchData tmData;
	
	private Integer buttonIDs[] = {
			R.id.btnAutoHiMiss,
			R.id.btnAutoLoMiss,
			R.id.btnAutoMove,
			R.id.btnAutoCollect,
			R.id.btnAutoDefend
	};
	
	private Hashtable<Integer, Button> buttonHash;
	private Hashtable<Integer, String> helpHash;
	
	protected int autoScore;
	protected TextView txtAutoScore;
	protected Switch switchUndo;
	
	protected Boolean undo;
	
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
    	
    	myTitle = "Auto Mode";
    	
    	this.teamMatchID = getArguments() != null ? getArguments().getLong("tmID") : -1;
    	
        View rootView = inflater.inflate(R.layout.fragment_team_match_automode, container, false);
        
        helpHash = new Hashtable<Integer, String>();
    	helpHash.put(R.id.btnAutoHiMiss, "Missing a shot aimed for the Hi Goal");
    	helpHash.put(R.id.btnAutoLoMiss, "Missing a shot aimed for the Lo Goal");
    	helpHash.put(R.id.btnAutoMove, "Moving out of the white zone (completely) away rom the truss.");
    	helpHash.put(R.id.btnAutoCollect, "Collecting a ball. Press button for each ball collected.");
    	helpHash.put(R.id.btnAutoDefend, "Defending in Goalie Zone during autonomous.");
        
        buttonHash = new Hashtable<Integer, Button>(); 

        txtAutoScore = (TextView) rootView.findViewById(R.id.txtAutoScore);

        switchUndo = (Switch) rootView.findViewById(R.id.switchUndo);
        switchUndo.setOnCheckedChangeListener(new OnCheckedChangeListener() {

			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				undo = isChecked;
				FTSUtilities.setButtonStyles(buttonHash, undo);
			}
        	
        });
        
        this.updateAutoScore();
        
        this.undo = false;
		
        //for(int ID : buttonIDs) {
	    //    buttonHash.put(ID, (Button) rootView.findViewById(ID));
	    //    buttonHash.get(ID).setOnClickListener(this);
        //}

        if(this.tmData.tabletID.startsWith("Red")) {
        	rootView.setBackgroundDrawable(getResources().getDrawable(R.drawable.automode_background_2015)); //.field_end_800_367_red));
        } else if(this.tmData.tabletID.startsWith("Blue")) {
        	rootView.setBackgroundDrawable(getResources().getDrawable(R.drawable.automode_background_2015)); //.field_end_800_367_blue));
        }

        rootView.findViewById(R.id.imgYellowTote1).setOnTouchListener(new MyTouchListener());
        rootView.findViewById(R.id.imgYellowTote2).setOnTouchListener(new MyTouchListener());
        rootView.findViewById(R.id.imgYellowTote3).setOnTouchListener(new MyTouchListener());
        rootView.findViewById(R.id.imgGreenCan1).setOnTouchListener(new MyTouchListener());
        rootView.findViewById(R.id.imgGreenCan2).setOnTouchListener(new MyTouchListener());
        rootView.findViewById(R.id.imgGreenCan3).setOnTouchListener(new MyTouchListener());
        rootView.findViewById(R.id.layoutRelative).setOnDragListener(new MyDragListener());

        return rootView;
    }

    private final class MyTouchListener implements View.OnTouchListener {
        @Override
        public boolean onTouch(View view, MotionEvent motionEvent) {
            if (motionEvent.getAction() == MotionEvent.ACTION_DOWN) {
                ClipData data = ClipData.newPlainText("", "");
                View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(view);
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
    
    static TeamMatchAutoModeFragment newInstance(Long teamMatchID) {
    	TeamMatchAutoModeFragment f = new TeamMatchAutoModeFragment();

        // Supply num input as an argument.
        Bundle args = new Bundle();
        args.putLong("tmID", teamMatchID);
        f.setArguments(args);

        return f;
    }
    
    @Override
    public void onClick(View v) {
		EnterTeamMatchDataActivity act = ((EnterTeamMatchDataActivity)this.getActivity());
		Integer btnID = v.getId();
		
		if(act.helpActive) {
			act.disableHelp();
			String message = this.helpHash.get(btnID);
			Toast.makeText(getActivity(), message, Toast.LENGTH_LONG).show();
		} else {
	        switch (btnID) {
	        case R.id.btnAutoHiMiss:
	        	btnAutoHiMissOnClick(v);
	        	break;
	        case R.id.btnAutoLoMiss:
	        	btnAutoLoMissOnClick(v);
	        	break;
	        case R.id.btnAutoCollect:
	        	btnAutoCollectOnClick(v);
	        	break;
	        case R.id.btnAutoMove:
	        	btnAutoMoveOnClick(v);
	        	break;
	        case R.id.btnAutoDefend:
	        	btnAutoDefendOnClick(v);
	        	break;
	        }
		}
    }
    
    public void setTeamMatchData(TeamMatchData tmD) {
    	this.tmData = tmD;
    }
    
	private void updateAutoScore() {
		if(this.txtAutoScore != null && this.tmData != null) {
			this.txtAutoScore.setText(String.valueOf(this.tmData.getAutoScore()));
		}
	}

	public void btnAutoHiScoreHotOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerAutoHiScore();
			savedData |= this.tmData.lowerHiHotBonus();
		} else {
			this.tmData.addAutoHiScore();
			this.tmData.addHiHotBonus();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnAutoHiScoreHotOnClick"); 
		updateAutoScore();
	}
	
	public void btnAutoHiScoreColdOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerAutoHiScore();
		} else {
			this.tmData.addAutoHiScore();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnAutoHiScoreColdOnClick");
		updateAutoScore();
	}
	
	public void btnAutoLoScoreHotOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerAutoLoScore();
			savedData |= this.tmData.lowerLoHotBonus();
		} else {
			this.tmData.addAutoLoScore();
			this.tmData.addLoHotBonus();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnAutoLoScoreHotOnClick");
		updateAutoScore();
	}
	
	public void btnAutoLoScoreColdOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerAutoLoScore();
		} else {
			this.tmData.addAutoLoScore();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnAutoLoScoreColdOnClick");
		updateAutoScore();
	}
	
	public void btnAutoHiMissOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerAutoHiMiss();
		} else {
			this.tmData.addAutoHiMiss();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnAutoHiMissOnClick");
	}
	
	public void btnAutoLoMissOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerAutoLoMiss();
		} else {
			this.tmData.addAutoLoMiss();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnAutoLoMissOnClick");
	}
	
	protected void btnAutoDefendOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerAutoDefend();	
		} else {
			this.tmData.addAutoDefend();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnAutoDefendOnClick");
	}

	protected void btnAutoMoveOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			this.tmData.didNotMoveInAuto();
			savedData = false;
		} else {
			this.tmData.movedInAuto();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnAutoMoveOnClick");
	}

	protected void btnAutoCollectOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerAutoCollect();
		} else {
			this.tmData.addAutoCollect();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnAutoCollectOnClick");
	}

    class MyDragListener implements View.OnDragListener {
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
                    FTSUtilities.printToConsole("TeamMatchStartingPositionFragment::DragEvent::ACTION_DROP\n");
                    View view = (View) event.getLocalState();

                    String toastText = "onDrag Event X: " + event.getX() + " Y: " + event.getY();
                    Toast.makeText(getActivity(), toastText, Toast.LENGTH_LONG).show();

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
