package com.wilsonvillerobotics.firstteamscouter;

import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
 
public class TabsPagerAdapter extends FragmentPagerAdapter {
 
	private Long teamMatchID;
	private Integer teamNumber;
	private Integer matchNumber;
	private TeamMatchData tmData;
	protected Boolean fieldOrientationRedOnRight;
	
	public static String tabTitles[] = { "1: Team #", "2: Position", "3: Auto", "4: Tele", "5: Notes" };
	
    public TabsPagerAdapter(FragmentManager fm, TeamMatchData tmD, Boolean fieldOrientationRedOnRight) {  //, Integer tmID, String tNum, String mNum) {
        super(fm);
        this.tmData = tmD;
        this.fieldOrientationRedOnRight = fieldOrientationRedOnRight;
        
        if(this.tmData != null) {
        	this.teamMatchID = tmData.teamMatchID;
            this.teamNumber = tmData.teamNumber;
            this.matchNumber = tmData.matchNumber;
        } else {
	        this.teamMatchID = Long.MIN_VALUE;
	        this.teamNumber = -1;
	        this.matchNumber = -1;
        	//this.teamMatchID = tmID;
	        //this.teamNumber = tNum;
	        //this.matchNumber = mNum;
        }
    }
 
    @Override
    public Fragment getItem(int index) {
 
        switch (index) {
        case 0:
        	TeamMatchTeamNumberDisplayFragment tmtndFrag = TeamMatchTeamNumberDisplayFragment.newInstance(this.teamMatchID);
        	tmtndFrag.setTeamMatchData(tmData);
        	return tmtndFrag;
        case 1:
        	//TeamMatchStartingPositionFragment tmspFrag = TeamMatchStartingPositionFragment.newInstance(this.teamMatchID);
        	//tmspFrag.setTeamMatchData(tmData);
        	//return tmspFrag;
        case 2:
            //TeamMatchAutoModeFragment tmaFrag = TeamMatchAutoModeFragment.newInstance(this.teamMatchID);
            //tmaFrag.setTeamMatchData(tmData);
            //return tmaFrag;
        case 3:
        	//TeamMatchTeleModeFragment tmtFrag =  TeamMatchTeleModeFragment.newInstance(this.teamMatchID);
        	//tmtFrag.setFieldOrientation(fieldOrientationRedOnRight);
        	//tmtFrag.setTeamMatchData(tmData);
        	//return tmtFrag;
        case 4:
        	//TeamMatchNotesFragment tmnFrag = TeamMatchNotesFragment.newInstance(this.teamMatchID, this.teamNumber, this.matchNumber);
        	//tmnFrag.setTeamMatchData(tmData);
        	//return tmnFrag;
        }
 
        return null;
    }
    
    public String getTabText(int index) {
        switch (index) {
        case 0:
        	return TeamMatchTeamNumberDisplayFragment.myTitle;
        case 1:
        	//return TeamMatchStartingPositionFragment.myTitle;
        case 2:
            //return TeamMatchAutoModeFragment.myTitle;
        case 3:
            //return TeamMatchTeleModeFragment.myTitle;
        case 4:
            //return TeamMatchNotesFragment.myTitle;
        }
 
        return null;
    }
 
    @Override
    public int getCount() {
        // get item count - equal to number of tabs
        return 5;
    }
 
}
