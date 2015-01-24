package com.wilsonvillerobotics.firstteamscouter;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.DBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.os.Bundle;
import android.app.Activity;
import android.preference.PreferenceFragment;
import android.preference.PreferenceManager;
import android.preference.SwitchPreference;
import android.content.Intent;
import android.content.SharedPreferences;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

public class MainActivity extends Activity {

	private Button btnViewTeamData;
	private Button btnImportMatchData;
	private Button btnSelectMatchTeam;
	private DBAdapter mDBAdapter;
	private TextView txtTabletID;
	private String tabletID;
	public Boolean fieldOrientRedOnRight;
	
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        FTSUtilities.printToConsole("Creating MainActivity");
        
        this.txtTabletID = (TextView) findViewById(R.id.txtTabletID);
        this.loadPref();
        
        this.mDBAdapter = new DBAdapter(this).open();
        
        btnViewTeamData = (Button) findViewById(R.id.btnViewTeamData);
        btnImportMatchData = (Button) findViewById(R.id.btnViewMatchData);
        btnSelectMatchTeam = (Button) findViewById(R.id.btnSelectMatchTeam);
        
        btnViewTeamData.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				Intent intent = new Intent(v.getContext(), TeamDataListActivity.class);
				intent.putExtra("tablet_id", tabletID);
				intent.putExtra("field_orientation", fieldOrientRedOnRight);
				startActivity(intent);
			}
		});
        
        btnImportMatchData.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				Intent intent = new Intent(v.getContext(), ImportMatchDataActivity.class);
				intent.putExtra("tablet_id", tabletID);
				intent.putExtra("field_orientation", fieldOrientRedOnRight);
				startActivity(intent);
			}
		});

        btnSelectMatchTeam.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				Intent intent = new Intent(v.getContext(), SelectMatchTeamActivity.class);
				intent.putExtra("tablet_id", tabletID);
				intent.putExtra("field_orientation", fieldOrientRedOnRight);
				startActivity(intent);
			}
		});
    }
    
    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
    }

    @Override
    protected void onStart() {
        super.onStart();
        //mDBAdapter.open();
    }

    @Override
    protected void onResume() {
        super.onResume();
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onPause() {
        super.onPause();
    }

    @Override
    protected void onStop() {
        super.onStop();
    }

    @Override
    protected void onDestroy() {
    	FTSUtilities.printToConsole("Destroying MainActivity");
    	mDBAdapter.close();
        super.onDestroy();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }
    
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
    	startActivityForResult(new Intent(this, SetPreferenceActivity.class), 0);
        return true;
//        switch (item.getItemId()) {
//            case 0:
//                startActivityForResult(new Intent(this, SetPreferenceActivity.class), 0);
//                return true;
//        }
//        return false;
    }
    
    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		// TODO Auto-generated method stub
		//super.onActivityResult(requestCode, resultCode, data);
		     
		/*
		* To make it simple, always re-load Preference setting.
		*/
		     
		loadPref();
    }
    
    private void loadPref() {
		SharedPreferences mySharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		
		this.fieldOrientRedOnRight = mySharedPreferences.getBoolean("field_orientation", false);
		this.tabletID = mySharedPreferences.getString("tablet_id_from_list", "Undefined Tablet ID");
		this.txtTabletID.setText(this.tabletID);
  	}

    public static class PrefsFragment extends PreferenceFragment {
    	 
        @Override
        public void onCreate(Bundle savedInstanceState) {
	        super.onCreate(savedInstanceState);
	
	        // Load the preferences from an XML resource
	        addPreferencesFromResource(R.xml.preferences);
        }
    }
}


