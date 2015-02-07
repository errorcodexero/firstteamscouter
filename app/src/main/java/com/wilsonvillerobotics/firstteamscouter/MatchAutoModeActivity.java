package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Rect;
import android.os.Bundle;
import android.view.ContextMenu;
import android.view.DragEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

public class MatchAutoModeActivity extends Activity {

	protected TeamMatchDBAdapter tmDBAdapter;
	protected String[] teamNumberArray;
    protected long teamMatchID;
	protected long teamID;
	protected long matchID;
	protected Button btnSubmit;
	private String tabletID;
    private int matchNumber;
	
	private ImageView imgYellowTote1;
    private ImageView imgYellowTote2;
    private ImageView imgYellowTote3;

    private ImageView imgAllianceCan1;
    private ImageView imgAllianceCan2;
    private ImageView imgAllianceCan3;

    private ImageView imgStepCan1;
    private ImageView imgStepCan2;
    private ImageView imgStepCan3;
    private ImageView imgStepCan4;

    private ImageView imgRobot;

	protected Boolean fieldOrientationRedOnRight;
    protected int startingRobotX;
    protected int startingRobotY;

    public int totesPickedUp;
    public int totesStacked;
    public int totesScored;
    public int cansPickedUp;
    public int cansScored;
    public int cansGrabbedFromStep;
    public int finalLocationX;
    public int finalLocationY;


	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_automode);

        RelativeLayout automodeParentLayout = (RelativeLayout) findViewById(R.id.AutoMode_Field_LayoutRelative);
        automodeParentLayout.setOnDragListener(new MyViewDragListener());
        processIntent(getIntent());
        setBackground(automodeParentLayout);
        configureTotesAndCans();

		teamID = -1;
		matchID = -1;
		
		try {
			FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
			tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).open();
		} catch(SQLException e) {
			e.printStackTrace();
			tmDBAdapter = null;
		}
		
		btnSubmit = (Button) findViewById(R.id.btnSubmitMatchAuto);
		btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
                //finish();
            }

            private void btnSubmitOnClick(View v) {
                FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate::btnSubmitMatchAuto : CLOSING DB\n");
                tmDBAdapter.close();

                Intent autoIntent = new Intent(v.getContext(), MatchTeleModeActivity.class);
                buildIntent(autoIntent);
                startActivity(autoIntent);
            }
        });
	}

    private void processIntent(Intent intent) {
        this.tabletID = intent.getStringExtra("tablet_id");
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
        this.startingRobotX = intent.getIntExtra("robot_x", 25);
        this.startingRobotY = intent.getIntExtra("robot_y", 25);
    }

    private void buildIntent(Intent intent) {
        intent.putExtra("tablet_id", tabletID);
        intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        intent.putExtra("match_number", matchNumber);
        intent.putExtra("tmID", teamMatchID);
        intent.putExtra("robot_x", startingRobotX);
        intent.putExtra("robot_y", startingRobotY);
    }

    private void setBackground(RelativeLayout automodeParentLayout) {
        int backgroundResource = R.drawable.automode_background_2015;
        if(this.tabletID.startsWith("Red")) {
            backgroundResource = R.drawable.automode_background_2015;
        } else if(this.tabletID.startsWith("Blue")) {
            backgroundResource = R.drawable.automode_background_2015;
        }
        automodeParentLayout.setBackgroundResource(backgroundResource);
    }

    private void createRobotImageView() {
        imgRobot = new ImageView(getBaseContext());
        imgRobot.setImageDrawable(getResources().getDrawable(R.drawable.robot_50x50));
        imgRobot.setOnTouchListener(new MyViewTouchListener());
        placeRobotOnScreen();
    }

    private void placeRobotOnScreen() {
        RelativeLayout relLayout = (RelativeLayout) findViewById(R.id.AutoMode_Field_LayoutRelative);
        relLayout.addView(imgRobot);
        this.setRobotLayout();
    }

    private void setRobotLayout() {
        int width = getResources().getDimensionPixelSize(R.dimen.robot_width);
        int height = getResources().getDimensionPixelSize(R.dimen.robot_height);
        int left = this.startingRobotX;
        int top  = this.startingRobotY;

        if(imgRobot == null) {
            imgRobot = (ImageView) findViewById(R.id.imgRobot);
        }
        setViewLayout(imgRobot, width, height, left, top);
    }

    private void setViewLayout(View v, int left, int top) {
        int height = v.getHeight();
        int width = v.getWidth();
        setViewLayout(v, width, height, left, top);
    }

    private void setViewLayout(View v, int width, int height, int left, int top) {
        RelativeLayout.LayoutParams viewLayoutParams = new RelativeLayout.LayoutParams(
                width,
                height
        );
        RelativeLayout parent = (RelativeLayout) v.getParent();
        int parentWidth = 0, parentHeight = 0;
        if(parent.getId() == R.id.AutoMode_Field_LayoutRelative) {
            parentWidth = getResources().getDimensionPixelSize(R.dimen.field_width);
            parentHeight = getResources().getDimensionPixelSize(R.dimen.field_height);
        } else if(parent != null) {
            parentWidth = parent.getMeasuredWidth();
            parentHeight = parent.getMeasuredHeight();
        }

        int maxLeft = parentWidth - width/2;
        int minLeft = width/2;
        int maxTop = parentHeight - height/2;
        int minTop = height/2;
        if(left > maxLeft) {
            left = maxLeft;
        }
        if(left < minLeft) {
            left = minLeft;
        }
        if(top > maxTop) {
            top = maxTop;
        }
        if(top < minTop) {
            top = minTop;
        }

        // Bottom Corner
        // y = -0.846 x + 600
        float bottomCorner = 709.1f - 1.182f*top;
        float topCorner = 1.273f*top + 259.64f;
        if(top >= 290 && left > bottomCorner) {
            left = (int)bottomCorner;
        }
        if(top <= 75 && left > topCorner) {
            left = (int)topCorner;
        }


        viewLayoutParams.setMargins(left - (width/2), top - (height/2), 0, 0);
        viewLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
        viewLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_TOP);
        v.setLayoutParams(viewLayoutParams);
    }

    private void configureTotesAndCans() {
        imgYellowTote1 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgYellowTote1.setOnTouchListener(new MyViewTouchListener());
        registerForContextMenu(imgYellowTote1);
        imgYellowTote2 = (ImageView) findViewById(R.id.imgYellowTote2);
        imgYellowTote2.setOnTouchListener(new MyViewTouchListener());
        registerForContextMenu(imgYellowTote2);
        imgYellowTote3 = (ImageView) findViewById(R.id.imgYellowTote3);
        imgYellowTote3.setOnTouchListener(new MyViewTouchListener());
        registerForContextMenu(imgYellowTote3);

        imgAllianceCan1 = (ImageView) findViewById(R.id.imgGreenCan1);
        imgAllianceCan1.setOnTouchListener(new MyViewTouchListener());
        imgAllianceCan2 = (ImageView) findViewById(R.id.imgGreenCan2);
        imgAllianceCan2.setOnTouchListener(new MyViewTouchListener());
        imgAllianceCan3 = (ImageView) findViewById(R.id.imgGreenCan3);
        imgAllianceCan3.setOnTouchListener(new MyViewTouchListener());

        imgStepCan1 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan1.setOnTouchListener(new MyViewTouchListener());
        imgStepCan2 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan2.setOnTouchListener(new MyViewTouchListener());
        imgStepCan3 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan3.setOnTouchListener(new MyViewTouchListener());
        imgStepCan4 = (ImageView) findViewById(R.id.imgYellowTote1);
        imgStepCan4.setOnTouchListener(new MyViewTouchListener());
    }

    private final class MyViewTouchListener implements View.OnTouchListener {
        @Override
        public boolean onTouch(View view, MotionEvent motionEvent) {
            if (motionEvent.getAction() == MotionEvent.ACTION_DOWN) {
                ClipData data = ClipData.newPlainText("", "");
                View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(view);
                view.startDrag(data, shadowBuilder, view, 0);
                return true;
            } else if (motionEvent.getAction() == MotionEvent.ACTION_MOVE) {
                view.setVisibility(View.GONE);
                return true;
            } else if (motionEvent.getAction() == MotionEvent.ACTION_UP) {
                return true;
            } else {
                return false;
            }
        }
    }

    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
        }
    }

    private void loadData() {
        if(this.tmDBAdapter != null) {
            Cursor C = this.tmDBAdapter.getStartingPosition(this.teamMatchID);
            if(C != null && C.getCount() > 0) {
                this.startingRobotX = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_START_LOCATION_X));
                //this.txtRobotX.setText(String.valueOf(this.startingRobotX));
                this.startingRobotY = C.getInt(C.getColumnIndex(tmDBAdapter.COLUMN_NAME_START_LOCATION_Y));
                //this.txtRobotY.setText(String.valueOf(this.startingRobotY));
            }
        }
    }

    private boolean saveData() {
        if(this.tmDBAdapter != null) {
            //teamMatchID, finalAutoModePositionX, finalAutoModePositionY,
            //totesPickedUp, cansPickedUp, totesStacked, totesScored, cansScored, cansGrabbedFromStep
            return this.tmDBAdapter.setAutoModeActions(this.teamMatchID, this.startingRobotX, this.startingRobotY,
                    0, 0, 0, 0, 0, 0);
        }
        return false;
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        if(tmDBAdapter == null) {
        	tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext());
        }
        tmDBAdapter.open();
    }

    @Override
    protected void onStart() {
        super.onStart();
    }

    @Override
    protected void onResume() {
        super.onResume();
        createRobotImageView();
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
        FTSUtilities.printToConsole("SelectTeamMatchActivity::onStop : CLOSING DB\n");
		tmDBAdapter.close();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }

    /*
     * This method is called when a context menu for the view about to be shown.
     */
    @Override
    public void onCreateContextMenu(ContextMenu menu, View v,
                                    ContextMenu.ContextMenuInfo menuInfo) {

        super.onCreateContextMenu(menu, v, menuInfo);
        if(v.getId()==R.id.imgYellowTote1)
        {
            // to set the context menu's header icon
            menu.setHeaderIcon(R.drawable.robot_50x50);

            // to set the context menu's title
            menu.setHeaderTitle("Yellow Tote 1");

            // to add a new item to the menu
            menu.add(0,0,0, "Stack Tote");
            menu.add(0,1,0,"Drop Tote");
        }

    }

     /*
     * This method is called when an item in a context menu is selected.
     *
     */
    @Override
    public boolean onContextItemSelected(MenuItem item) {
        switch(item.getItemId())
        {
            case 0:
                stack(item.getItemId());
                break;
            case 1:
                drop(item.getItemId());
                break;
        }
        return true;
    }

    private void drop(int itemId) {
        Toast.makeText(this, "You pressed Drop", Toast.LENGTH_LONG).show();

    }

    public void stack(int i)
    {
        Toast.makeText(this, "You pressed Stack", Toast.LENGTH_LONG).show();
    }

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.enter_data, menu);
		return true;
	}

    private class MyViewDragListener implements View.OnDragListener {
        boolean dragging = false;
        @Override
        public boolean onDrag(View v, DragEvent event) {
            int action = event.getAction();
            switch (action) {
                case DragEvent.ACTION_DRAG_STARTED:
                    dragging = true;
                    break;
                case DragEvent.ACTION_DRAG_ENTERED:
                    //v.setBackgroundDrawable(enterShape);
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    //v.setBackgroundDrawable(normalShape);
                    break;
                case DragEvent.ACTION_DROP:
                    // Dropped, reassign View to ViewGroup
                    dragging = false;
                    View view = (View) event.getLocalState();

                    ViewGroup owner = (ViewGroup) view.getParent();
                    owner.removeView(view);
                    RelativeLayout container = (RelativeLayout) v;
                    container.addView(view);

                    int left = (int) event.getX();
                    int top = (int) event.getY();
                    if(view.getId() == R.id.imgRobot) {
                        startingRobotX = left;
                        startingRobotY = top;
                        setRobotLayout();
                    } else {
                        setViewLayout(view, left, top);
                        boolean yellowTote1 = (view.getId() == R.id.imgYellowTote1);
                        Rect viewRect = new Rect();
                        viewRect.left = left;
                        viewRect.top = top;
                        viewRect.right = left + view.getWidth();
                        viewRect.bottom = top + view.getHeight();

                        Rect r1 = new Rect();
                        Rect r2 = new Rect();
                        Rect r3 = new Rect();

                        imgYellowTote1.getHitRect(r1);
                        imgYellowTote2.getHitRect(r2);
                        imgYellowTote3.getHitRect(r3);

                        if(Rect.intersects(viewRect, r1)) {
                            Toast.makeText(getApplicationContext(), "On top of Yellow Tote 1", Toast.LENGTH_LONG).show();
                            view.showContextMenu();
                        } else if(Rect.intersects(viewRect, r2)) {
                            Toast.makeText(getApplicationContext(), "On top of Yellow Tote 2", Toast.LENGTH_LONG).show();
                            view.showContextMenu();
                        } else if(Rect.intersects(viewRect, r3)) {
                            Toast.makeText(getApplicationContext(), "On top of Yellow Tote 3", Toast.LENGTH_LONG).show();
                            view.showContextMenu();
                        }
                    }
                    view.setVisibility(View.VISIBLE);

                    //txtRobotX.setText(String.valueOf(startingRobotX));
                    //txtRobotY.setText(String.valueOf(startingRobotY));
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    if(dragging) {
                        startingRobotX = (int) event.getX();
                        startingRobotY = (int) event.getY();
                        //txtRobotX.setText(String.valueOf(startingRobotX));
                        //txtRobotY.setText(String.valueOf(startingRobotY));
                    }
                    break;
                default:
                    break;
            }
            return true;
        }
    }
}
