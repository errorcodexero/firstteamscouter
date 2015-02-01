package com.wilsonvillerobotics.firstteamscouter;

import java.util.Hashtable;

import com.wilsonvillerobotics.firstteamscouter.TeamMatchData.ZONE;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.os.Bundle;
import android.view.InflateException;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.GridLayout;
import android.widget.Toast;
import android.widget.GridLayout.Spec;
import android.widget.Switch;
import android.widget.TextView;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.support.v4.app.Fragment;

public class TeamMatchTeleModeFragment extends Fragment implements OnClickListener{
	public static String myTitle;
	protected Long teamMatchID;
	private TeamMatchData tmData;
	protected Boolean fieldOrientationRedOnRight;
	
	protected int teleScore;
	protected TextView txtTeleScore;
	protected TextView txtTeleStat;
	protected GridLayout gridTeleModeLayout;

	private Integer buttonIDs[] = {
			R.id.btnTeleHiScore,
			R.id.btnTeleHiMiss,
			R.id.btnTeleLoScore,
			R.id.btnTeleLoMiss,
			R.id.btnTeleLongPass,
			R.id.btnTeleLongPassMiss,
			R.id.btnDefendBlueZone,
			R.id.btnDefendWhiteZone,
			R.id.btnDefendRedZone,
			R.id.btnDefendGoalZone,
			R.id.btnPossessBlueZone,
			R.id.btnPossessWhiteZone,
			R.id.btnPossessRedZone,
			R.id.btnTrussToss,
			R.id.btnTrussMiss,
			R.id.btnTossCaught,
			R.id.btnTossMiss,
			R.id.btnTeleShortPass,
			R.id.btnTeleShortPassMiss
	};
	
	private Hashtable<Integer, Button> buttonHash;
	private Hashtable<Integer, String> helpHash;
	
	protected Switch switchUndo;
	
	protected Boolean undo;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
    	
    	myTitle = "Tele Mode";

    	this.teamMatchID = getArguments() != null ? getArguments().getLong("tmID") : -1;

        View rootView = null;
        try {
            rootView = inflater.inflate(R.layout.fragment_team_match_telemode, container, false);

            helpHash = new Hashtable<Integer, String>();
            helpHash.put(R.id.btnTeleHiScore,"Scoring in the Hi goal");
            helpHash.put(R.id.btnTeleHiMiss,"Missing a shot aimed at the Hi goal");
            helpHash.put(R.id.btnTeleLoScore,"Scoring in the Lo goal");
            helpHash.put(R.id.btnTeleLoMiss,"Missing a shot aimed at the Lo goal");
            helpHash.put(R.id.btnTeleLongPass,"Accurate pass across a long distance");
            helpHash.put(R.id.btnTeleLongPassMiss,"Missing a pass across a long distance");
            helpHash.put(R.id.btnDefendBlueZone,"Successfully defending in the Blue zone - blocking a pass, shot, or robot.");
            helpHash.put(R.id.btnDefendWhiteZone,"Successfully defending in the White zone - blocking a pass, shot, or robot.");
            helpHash.put(R.id.btnDefendRedZone,"Successfully defending in the Red zone - blocking a pass, shot, or robot.");
            helpHash.put(R.id.btnDefendGoalZone,"Successfully defending in the Goalie zone - blocking a shot aimed at the Hi goal.");
            helpHash.put(R.id.btnPossessBlueZone,"Successfully possessing the ball in the Blue zone");
            helpHash.put(R.id.btnPossessWhiteZone,"Successfully possessing the ball in the White zone");
            helpHash.put(R.id.btnPossessRedZone,"Successfully possessing the ball in the Red zone");
            helpHash.put(R.id.btnTrussToss,"Successful toss over the truss");
            helpHash.put(R.id.btnTrussMiss,"Failure to make a toss over the truss (bounced off, went out of bounds, too low, etc)");
            helpHash.put(R.id.btnTossCaught,"Successfully caught a toss over the truss!");
            helpHash.put(R.id.btnTossMiss,"Missed catching a toss over the truss");
            helpHash.put(R.id.btnTeleShortPass,"Accurate pass over short distance - usually two robots almost touching");
            helpHash.put(R.id.btnTeleShortPassMiss,"Missing a pass to another robot at a small distance");

            this.txtTeleScore = (TextView) rootView.findViewById(R.id.txtTeleScore);
            this.txtTeleStat = (TextView) rootView.findViewById(R.id.txtTeleStat);

            this.updateTeleScore();

            this.undo = false;

            buttonHash = new Hashtable<Integer, Button>();

            switchUndo = (Switch) rootView.findViewById(R.id.switchUndo);
            switchUndo.setOnCheckedChangeListener(new OnCheckedChangeListener() {

                @Override
                public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                    undo = isChecked;
                    //setButtonBackgrounds();
                    FTSUtilities.setButtonStyles(buttonHash, undo);
                }

            });

            for(int ID : buttonIDs) {
                buttonHash.put(ID, (Button) rootView.findViewById(ID));
                buttonHash.get(ID).setOnClickListener(this);
            }

            this.gridTeleModeLayout = (GridLayout) rootView.findViewById(R.id.gridTeleModeLayout);


            Spec colSpecLeft = GridLayout.spec(5, 1);
            Spec colSpecRight = GridLayout.spec(8, 2);

            Button bluePossess = buttonHash.get(R.id.btnPossessBlueZone);
            Button redPossess = buttonHash.get(R.id.btnPossessRedZone);
            Button blueDefend = buttonHash.get(R.id.btnDefendBlueZone);
            Button redDefend = buttonHash.get(R.id.btnDefendRedZone);

            GridLayout.LayoutParams bluePossessLP = (GridLayout.LayoutParams)bluePossess.getLayoutParams();
            GridLayout.LayoutParams redPossessLP = (GridLayout.LayoutParams)redPossess.getLayoutParams();
            GridLayout.LayoutParams blueDefendLP = (GridLayout.LayoutParams)blueDefend.getLayoutParams();
            GridLayout.LayoutParams redDefendLP = (GridLayout.LayoutParams)redDefend.getLayoutParams();

            if(fieldOrientationRedOnRight) {
                rootView.setBackgroundDrawable(getResources().getDrawable(R.drawable.field_800x367_blue_on_left));

                bluePossessLP.columnSpec = colSpecLeft;
                blueDefendLP.columnSpec = colSpecLeft;

                redPossessLP.columnSpec = colSpecRight;
                redDefendLP.columnSpec = colSpecRight;
            } else {
                rootView.setBackgroundDrawable(getResources().getDrawable(R.drawable.field_800x367_red_on_left));

                bluePossessLP.columnSpec = colSpecRight;
                blueDefendLP.columnSpec = colSpecRight;

                redPossessLP.columnSpec = colSpecLeft;
                redDefendLP.columnSpec = colSpecLeft;
            }

            bluePossess.setLayoutParams(bluePossessLP);
            redPossess.setLayoutParams(redPossessLP);
            blueDefend.setLayoutParams(blueDefendLP);
            redDefend.setLayoutParams(redDefendLP);
        }
        catch(InflateException ie) {
            FTSUtilities.printToConsole("InflateException::message: " + ie.getMessage());
        }
        return rootView;
    }
    
    static TeamMatchTeleModeFragment newInstance(Long teamMatchID) {
    	TeamMatchTeleModeFragment f = new TeamMatchTeleModeFragment();

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
		EnterTeamMatchDataActivity act = ((EnterTeamMatchDataActivity)this.getActivity());
		Integer btnID = v.getId();
		
		if(act.helpActive) {
			act.disableHelp();
			String message = this.helpHash.get(btnID);
			Toast.makeText(getActivity(), message, Toast.LENGTH_LONG).show();
		} else {
			switch (btnID) {
	        case R.id.btnTeleHiScore:
				btnTeleHighScoreOnClick(v);
				break;
			case R.id.btnTeleLoScore:
				btnTeleLowScoreOnClick(v);
				break;
			case R.id.btnTeleHiMiss:
				btnTeleHiMissOnClick(v);
				break;
			case R.id.btnTeleLoMiss:
				btnTeleLoMissOnClick(v);
				break;
			case R.id.btnTrussToss:
				btnTrussTossOnClick(v);
				break;
			case R.id.btnTrussMiss:
				btnTrussMissOnClick(v);
				break;
			case R.id.btnTossCaught:
				btnTossCaughtOnClick(v);
				break;
			case R.id.btnTossMiss:
				btnTossMissedOnClick(v);
				break;
			case R.id.btnDefendRedZone:
				btnDefendRedZoneOnClick(v);
				break;
			case R.id.btnDefendWhiteZone:
				btnDefendWhiteZoneOnClick(v);
				break;
			case R.id.btnDefendBlueZone:
				btnDefendBlueZoneOnClick(v);
			 	break;
			case R.id.btnTeleLongPass:
				btnTeleLongPassOnClick(v);
				break;
			case R.id.btnTeleLongPassMiss:
				btnTeleLongPassMissOnClick(v);
				break;
			case R.id.btnTeleShortPass:
				btnTeleShortPassOnClick(v);
				break;
			case R.id.btnTeleShortPassMiss:
				btnTeleShortPassMissOnClick(v);
				break;
			}
		}
	}
	
	protected void btnTeleHighScoreOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleHiScore();
		} else {
			this.tmData.addTeleHiScore();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTeleHighScoreOnClick");
		this.updateTeleScore();
	}

	public void btnTeleHiMissOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleHiMiss();
		} else {
			this.tmData.addTeleHiMiss();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTeleHiMissOnClick");
		this.updateTeleStat();
	}
	
	protected void btnTeleLowScoreOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleLoScore();
		} else {
			this.tmData.addTeleLoScore();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTeleLowScoreOnClick");
		this.updateTeleScore();
	}

	public void btnTeleLoMissOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleLoMiss();
		} else {
			this.tmData.addTeleLoMiss();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTeleLoMissOnClick");
		this.updateTeleStat();
	}
	
	protected void btnTeleLongPassOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleLongPass();
		} else {
			this.tmData.addTeleLongPass();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTeleLongPassOnClick");
	}
	
	protected void btnTeleLongPassMissOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleLongPassMiss();
		} else {
			this.tmData.addTeleLongPassMiss();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTeleLongPassMissOnClick");
	}
	
	protected void btnTeleShortPassOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleShortPass();
		} else {
			this.tmData.addTeleShortPass();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTeleShortPassOnClick");
	}
	
	protected void btnTeleShortPassMissOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleShortPassMiss();
		} else {
			this.tmData.addTeleShortPassMiss();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTeleShortPassMissOnClick");
	}
	
	protected void btnPossessBlueZoneOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.didNotPossessZone(ZONE.BLUE_ZONE);
		} else {
			this.tmData.possessedZone(ZONE.BLUE_ZONE);
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnPossessBlueZoneOnClick");
	}

	protected void btnPossessedWhiteZoneOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.didNotPossessZone(ZONE.WHITE_ZONE);
		} else {
			this.tmData.possessedZone(ZONE.WHITE_ZONE);
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnPossessedWhiteZoneOnClick");
	}

	protected void btnPossessedRedZoneOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.didNotPossessZone(ZONE.RED_ZONE);
		} else {
			this.tmData.possessedZone(ZONE.RED_ZONE);
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnPossessedRedZoneOnClick");
	}

	protected void btnDefendBlueZoneOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.didNotDefendZone(ZONE.BLUE_ZONE);
		} else {
			this.tmData.defendedZone(ZONE.BLUE_ZONE);
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnDefendBlueZoneOnClick");
	}

	protected void btnDefendWhiteZoneOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.didNotDefendZone(ZONE.WHITE_ZONE);
		} else {
			this.tmData.defendedZone(ZONE.WHITE_ZONE);
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnDefendWhiteZoneOnClick");
	}

	protected void btnDefendRedZoneOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.didNotDefendZone(ZONE.RED_ZONE);
		} else {
			this.tmData.defendedZone(ZONE.RED_ZONE);
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnDefendRedZoneOnClick");
	}

	protected void btnDefendGoalZoneOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.didNotDefendZone(ZONE.GOAL_ZONE);
		} else {
			this.tmData.defendedZone(ZONE.GOAL_ZONE);
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnDefendGoalZoneOnClick");
	}

	protected void btnTrussTossOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleTrussToss();
		} else {
			this.tmData.addTeleTrussToss();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTrussTossOnClick");
	}

	protected void btnTrussMissOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleTrussMiss();
		} else {
			this.tmData.addTeleTrussMiss();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTrussMissOnClick");
	}

	protected void btnTossCaughtOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleTossCatch();
		} else {
			this.tmData.addTeleTossCatch();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTossCaughtOnClick");
	}

	protected void btnTossMissedOnClick(View v) {
		boolean savedData = false;
		if(this.undo) {
			savedData = this.tmData.lowerTeleTossMiss();
		} else {
			this.tmData.addTeleTossMiss();
			savedData = true;
		}
		this.tmData.setSavedDataState(savedData, "btnTossMissedOnClick");
	}

	private void updateTeleScore() {
		if(this.txtTeleScore != null && this.tmData != null) {
			this.txtTeleScore.setText(String.valueOf(this.tmData.getTeleScore()));
			this.updateTeleStat();
		}
	}

	protected void updateTeleStat() {
		if(this.txtTeleStat != null && this.tmData != null) {
			this.txtTeleStat.setText(String.format("%.2f%%", this.tmData.getTeleShotPercentage()));
		}
	}

	public void setFieldOrientation(Boolean fieldOrientationRedOnRight) {
		FTSUtilities.printToConsole("TeamMatchTeleModeFragment::setFieldOrientation : red on right: " + fieldOrientationRedOnRight.toString());
		this.fieldOrientationRedOnRight = fieldOrientationRedOnRight;
	}
}
