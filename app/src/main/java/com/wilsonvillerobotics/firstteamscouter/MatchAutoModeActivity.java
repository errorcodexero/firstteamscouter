package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Point;
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

import java.util.HashMap;

public class MatchAutoModeActivity extends Activity {

    public enum AutoFieldObject {
        Robot(R.id.imgRobot, GameElement.GameElementType.ROBOT, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_VISIBLE),
        YellowTote1(R.id.imgYellowTote1, GameElement.GameElementType.YELLOW_TOTE, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE1_VISIBLE),
        YellowTote2(R.id.imgYellowTote2, GameElement.GameElementType.YELLOW_TOTE, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE2_VISIBLE),
        YellowTote3(R.id.imgYellowTote3, GameElement.GameElementType.YELLOW_TOTE, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE3_VISIBLE),
        GreenCan1(R.id.imgGreenCan1, GameElement.GameElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN1_VISIBLE),
        GreenCan2(R.id.imgGreenCan2, GameElement.GameElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN2_VISIBLE),
        GreenCan3(R.id.imgGreenCan3, GameElement.GameElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN3_VISIBLE),
        GreenCan4(R.id.imgGreenCan4, GameElement.GameElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN4_VISIBLE),
        GreenCan5(R.id.imgGreenCan5, GameElement.GameElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_5_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_5_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN5_VISIBLE),
        GreenCan6(R.id.imgGreenCan6, GameElement.GameElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_6_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_6_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN6_VISIBLE),
        GreenCan7(R.id.imgGreenCan7, GameElement.GameElementType.CAN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_7_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_7_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN7_VISIBLE);

        private int id;
        private String dbColumnX, dbColumnY, dbVisible;
        private GameElement.GameElementType type;

        AutoFieldObject(int id, GameElement.GameElementType et, String colX, String colY, String visible) {
            this.id = id;
            this.dbColumnX = colX;
            this.dbColumnY = colY;
            this.dbVisible = visible;
            this.type = et;
        }

        public int getId() {
            return this.id;
        }

        public String dbLocX() {
            return this.dbColumnX;
        }

        public String dbLocY() {
            return this.dbColumnY;
        }

        public String dbVisible() {
            return this.dbVisible;
        }

        public GameElement.GameElementType getType() {
            return this.type;
        }
    }

	protected TeamMatchDBAdapter tmDBAdapter;
	protected String[] teamNumberArray;
    protected long teamMatchID;
	protected long teamID;
	protected long matchID;
	protected Button btnSubmit;
	//private String tabletID;
    private FTSUtilities.ALLIANCE_POSITION tabletAlliancePosition;
    private int matchNumber;
    private View lastViewTouched;
    private GameElement lastElementCollided;

	protected Boolean fieldOrientationRedOnRight;
    protected Point autoRobotStartingLocation;

    protected HashMap<Integer, GameElement> autoFieldObjects;

    public int totesPickedUp;
    public int totesStacked;
    public int totesScored;
    public int cansPickedUp;
    public int cansScored;
    public int cansGrabbedFromStep;

    protected boolean autoModeSaved;


	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_automode);

        this.autoModeSaved = false;

        RelativeLayout automodeParentLayout = (RelativeLayout) findViewById(R.id.AutoMode_Field_LayoutRelative);
        automodeParentLayout.setOnDragListener(new MyViewDragListener());

        this.autoRobotStartingLocation = new Point();
        this.autoFieldObjects = new HashMap<Integer, GameElement>();

        for(AutoFieldObject fo : AutoFieldObject.values()) {
            GameElement ge = new GameElement();
            ge.setId(fo.getId());
            ge.setLocation(new Point());
            ge.setElementType(fo.getType());

            ImageView iv = (ImageView)findViewById(fo.getId());
            if(iv == null) {
                iv = new ImageView(getBaseContext());
                iv.setId(fo.getId());
            }
            ge.setImageView(iv);
            ge.makeVisible();

            this.autoFieldObjects.put(fo.getId(), ge);
        }

        processIntent(getIntent());
        setBackground(automodeParentLayout);
        configureTotesAndCans();

        lastViewTouched = null;
        lastElementCollided = null;
		teamID = -1;
		matchID = -1;

        this.openDatabase();


        configureSubmitButton();
		/*
		btnSubmit = (Button) findViewById(R.id.btnSubmitMatchAuto);
		btnSubmit.setOnClickListener(new View.OnClickListener() {
		    @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
                //finish();
            }

            private void btnSubmitOnClick(View v) {
                FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate::btnSubmitMatchAuto : CLOSING DB\n");

                Intent autoIntent = new Intent(v.getContext(), MatchTeleModeActivity.class);
                buildIntent(autoIntent);
                startActivity(autoIntent);
            }
        });
        */
	}

    private void processIntent(Intent intent) {
        this.tabletAlliancePosition = FTSUtilities.ALLIANCE_POSITION.getAlliancePositionForString(intent.getStringExtra("tablet_id"));
        //this.tabletID = intent.getStringExtra("tablet_id");
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
        this.autoRobotStartingLocation.x = intent.getIntExtra("robot_x", 25);
        this.autoRobotStartingLocation.y = intent.getIntExtra("robot_y", 25);
    }

    private void buildIntent(Intent intent) {
        //intent.putExtra("tablet_id", tabletID);
        intent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
        intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        intent.putExtra("match_number", matchNumber);
        intent.putExtra("tmID", teamMatchID);
    }

    private void configureSubmitButton() {
        btnSubmit = (Button) findViewById(R.id.btnSubmitMatchAuto);
        btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
                //finish();
            }

            private void btnSubmitOnClick(View v) {
                Intent autoIntent = new Intent(v.getContext(), MatchTeleModeActivity.class);
                buildIntent(autoIntent);
                startActivity(autoIntent);
            }
        });
    }

    private void setBackground(RelativeLayout automodeParentLayout) {
        int backgroundResource = R.drawable.automode_background_2015;

        switch(tabletAlliancePosition) {
            case RED1:
            case RED2:
            case RED3:
                backgroundResource = R.drawable.auto_mode_red_field_500x500;
                break;
            case BLUE1:
            case BLUE2:
            case BLUE3:
                backgroundResource = R.drawable.auto_mode_blue_field_500x500;
                break;
            default:
                backgroundResource = R.drawable.automode_background_2015;
                break;
        }
        /*
        if(this.tabletID.startsWith("Red")) {
            backgroundResource = R.drawable.automode_background_2015;
        } else if(this.tabletID.startsWith("Blue")) {
            backgroundResource = R.drawable.automode_background_2015;
        }
        */
        automodeParentLayout.setBackgroundResource(backgroundResource);
    }

    private void createRobotImageView() {
        ImageView imgRobot = this.autoFieldObjects.get(AutoFieldObject.Robot.getId()).getImageView();
        if(imgRobot == null) {
            imgRobot = new ImageView(getBaseContext());
            imgRobot.setId(AutoFieldObject.Robot.getId());
            imgRobot.setImageDrawable(getResources().getDrawable(R.drawable.robot_50x50));
            imgRobot.setOnTouchListener(new MyViewTouchListener());
            registerForContextMenu(imgRobot);
            this.autoFieldObjects.get(AutoFieldObject.Robot.getId()).setImageView(imgRobot);
        } else if(imgRobot.getDrawable() == null) {
            imgRobot.setImageDrawable(getResources().getDrawable(R.drawable.robot_50x50));
            imgRobot.setOnTouchListener(new MyViewTouchListener());
            registerForContextMenu(imgRobot);
        }

        placeRobotOnScreen();
    }

    private void placeRobotOnScreen() {
        ImageView imgRobot = this.autoFieldObjects.get(AutoFieldObject.Robot.getId()).getImageView();
        ViewGroup parent = (ViewGroup)imgRobot.getParent();
        if(parent == null) {
            RelativeLayout relLayout = (RelativeLayout) findViewById(R.id.AutoMode_Field_LayoutRelative);
            relLayout.addView(imgRobot);
        } else {
            if(parent.getId() != R.id.AutoMode_Field_LayoutRelative) {
                parent.removeView(imgRobot);
            }
        }

        this.setInitialRobotLayout();
    }

    private void setInitialRobotLayout() {
        int width = getResources().getDimensionPixelSize(R.dimen.robot_width);
        int height = getResources().getDimensionPixelSize(R.dimen.robot_height);

        Point robotFinalLocation = this.autoFieldObjects.get(AutoFieldObject.Robot.getId()).getLocation();
        int left = (robotFinalLocation.x == -1) ? this.autoRobotStartingLocation.x : robotFinalLocation.x;
        int top  = (robotFinalLocation.y == -1) ? this.autoRobotStartingLocation.y : robotFinalLocation.y;

        ImageView imgRobot = this.autoFieldObjects.get(AutoFieldObject.Robot.getId()).getImageView();

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
        int parentWidth, parentHeight;
        if(parent.getId() == R.id.AutoMode_Field_LayoutRelative) {
            parentWidth = getResources().getDimensionPixelSize(R.dimen.field_width);
            parentHeight = getResources().getDimensionPixelSize(R.dimen.field_height);
        } else {
            parentWidth = parent.getMeasuredWidth();
            parentHeight = parent.getMeasuredHeight();
        }

        /// TODO: Look into using the hit rectangle for collision detection
        //Rect parentRect = new Rect();
        //parent.getDrawingRect(parentRect);

        int maxLeft = parentWidth - (width / 2);
        int minLeft = width/2;
        int maxTop = parentHeight - (height / 2);
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
        ImageView iv;
        for(AutoFieldObject fo : AutoFieldObject.values()) {
            if(fo.getId() != AutoFieldObject.Robot.getId()) {
                iv = this.autoFieldObjects.get(fo.getId()).getImageView();
                int visibility = (this.autoFieldObjects.get(fo.getId()).isVisible()) ? View.VISIBLE : View.INVISIBLE;
                iv.setVisibility(visibility);
                iv.setOnTouchListener(new MyViewTouchListener());
                registerForContextMenu(iv);
            }
        }
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
            } else return motionEvent.getAction() == MotionEvent.ACTION_UP;
        }
    }

    private void parseRobotStackList(String stackList) {
        String[] stackArray = stackList.split(" ");
        String msg = "";
        for(String s : stackArray) {
            if(s != null && s != "") {
                GameElement ge = autoFieldObjects.get(Integer.parseInt(s));
                autoFieldObjects.get(AutoFieldObject.Robot.getId()).pushToStack(ge);
                msg += ge.getElementType().toString();
                msg += "\n";
            }
        }
        Toast.makeText(getBaseContext(), msg, Toast.LENGTH_LONG).show();
    }

    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("MatchAutoModeActivity::onCreate : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
        }
    }

    private void loadData() {
        openDatabase();
        if(this.tmDBAdapter != null) {
            Cursor C = this.tmDBAdapter.getAutoModeData(this.teamMatchID);
            if(C.moveToFirst()) {
                this.autoModeSaved = Boolean.parseBoolean(C.getString(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_MODE_SAVED)));
                if (this.autoModeSaved) {
                    this.autoRobotStartingLocation.x = C.getInt(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X));
                    this.autoRobotStartingLocation.y = C.getInt(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y));

                    for(AutoFieldObject fo : AutoFieldObject.values()) {
                        GameElement ge = this.autoFieldObjects.get(fo.getId());
                        Point loc = ge.getLocation();
                        loc.x     = C.getInt(C.getColumnIndex(fo.dbLocX()));
                        loc.y     = C.getInt(C.getColumnIndex(fo.dbLocY()));
                        ge.setVisibility(Boolean.parseBoolean(C.getString(C.getColumnIndex(fo.dbVisible()))));
                    }

                    this.totesPickedUp = C.getInt(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTES_PICKED_UP));
                    this.cansPickedUp = C.getInt(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_CANS_PICKED_UP));
                    this.totesStacked = C.getInt(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTES_STACKED));
                    this.totesScored = C.getInt(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTES_SCORED));
                    this.cansScored = C.getInt(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_CANS_SCORED));
                    this.cansGrabbedFromStep = C.getInt(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP));

                    parseRobotStackList(C.getString(C.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_STACK_LIST)));
                } else {
                    Point robotFinalLocation = this.autoFieldObjects.get(AutoFieldObject.Robot.getId()).getLocation();
                    robotFinalLocation.x = -1;
                    robotFinalLocation.y = -1;

                    for(AutoFieldObject fo : AutoFieldObject.values()) {
                        GameElement ge = this.autoFieldObjects.get(fo.getId());
                        ge.setVisibility(true);
                    }

                    this.totesPickedUp = 0;
                    this.cansPickedUp = 0;
                    this.totesStacked = 0;
                    this.totesScored = 0;
                    this.cansScored = 0;
                    this.cansGrabbedFromStep = 0;
                }
            }
        }
    }

    private boolean saveData() {
        Point robotFinalLocation = this.autoFieldObjects.get(AutoFieldObject.Robot.getId()).getLocation();
        Point tote1FinalLocation = this.autoFieldObjects.get(AutoFieldObject.YellowTote1.getId()).getLocation();
        Point tote2FinalLocation = this.autoFieldObjects.get(AutoFieldObject.YellowTote2.getId()).getLocation();
        Point tote3FinalLocation = this.autoFieldObjects.get(AutoFieldObject.YellowTote3.getId()).getLocation();
        Point can1FinalLocation = this.autoFieldObjects.get(AutoFieldObject.GreenCan1.getId()).getLocation();
        Point can2FinalLocation = this.autoFieldObjects.get(AutoFieldObject.GreenCan2.getId()).getLocation();
        Point can3FinalLocation = this.autoFieldObjects.get(AutoFieldObject.GreenCan3.getId()).getLocation();
        Point can4FinalLocation = this.autoFieldObjects.get(AutoFieldObject.GreenCan4.getId()).getLocation();
        Point can5FinalLocation = this.autoFieldObjects.get(AutoFieldObject.GreenCan5.getId()).getLocation();
        Point can6FinalLocation = this.autoFieldObjects.get(AutoFieldObject.GreenCan6.getId()).getLocation();
        Point can7FinalLocation = this.autoFieldObjects.get(AutoFieldObject.GreenCan7.getId()).getLocation();
        boolean robotVisibility = this.autoFieldObjects.get(AutoFieldObject.Robot.getId()).isVisible();
        boolean tote1Visibility = this.autoFieldObjects.get(AutoFieldObject.YellowTote1.getId()).isVisible();
        boolean tote2Visibility = this.autoFieldObjects.get(AutoFieldObject.YellowTote2.getId()).isVisible();
        boolean tote3Visibility = this.autoFieldObjects.get(AutoFieldObject.YellowTote3.getId()).isVisible();
        boolean can1Visibility = this.autoFieldObjects.get(AutoFieldObject.GreenCan1.getId()).isVisible();
        boolean can2Visibility = this.autoFieldObjects.get(AutoFieldObject.GreenCan2.getId()).isVisible();
        boolean can3Visibility = this.autoFieldObjects.get(AutoFieldObject.GreenCan3.getId()).isVisible();
        boolean can4Visibility = this.autoFieldObjects.get(AutoFieldObject.GreenCan4.getId()).isVisible();
        boolean can5Visibility = this.autoFieldObjects.get(AutoFieldObject.GreenCan5.getId()).isVisible();
        boolean can6Visibility = this.autoFieldObjects.get(AutoFieldObject.GreenCan6.getId()).isVisible();
        boolean can7Visibility = this.autoFieldObjects.get(AutoFieldObject.GreenCan7.getId()).isVisible();
        String robotStackList = this.autoFieldObjects.get(AutoFieldObject.Robot.getId()).getStackList();

        return this.tmDBAdapter != null &&
                this.tmDBAdapter.setAutoModeActions(
                this.teamMatchID, robotFinalLocation.x, robotFinalLocation.y,
                tote1FinalLocation.x, tote1FinalLocation.y,
                tote2FinalLocation.x, tote2FinalLocation.y,
                tote3FinalLocation.x, tote3FinalLocation.y,
                can1FinalLocation.x, can1FinalLocation.y,
                can2FinalLocation.x, can2FinalLocation.y,
                can3FinalLocation.x, can3FinalLocation.y,
                can4FinalLocation.x, can4FinalLocation.y,
                can5FinalLocation.x, can5FinalLocation.y,
                can6FinalLocation.x, can6FinalLocation.y,
                can7FinalLocation.x, can7FinalLocation.y,
                robotVisibility, tote1Visibility, tote2Visibility, tote3Visibility,
                can1Visibility, can2Visibility, can3Visibility, can4Visibility,
                can5Visibility, can6Visibility, can7Visibility,
                this.totesPickedUp, this.totesStacked, this.totesScored,
                this.cansPickedUp, cansScored, cansGrabbedFromStep,
                robotStackList
                );
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
        loadData();
        createRobotImageView();
        layoutTotesAndCans();
    }

    private void layoutTotesAndCans() {
        if(autoModeSaved) {
            for(AutoFieldObject fo : AutoFieldObject.values()) {
                if(fo.getId() != AutoFieldObject.Robot.getId()) {
                    GameElement ge = autoFieldObjects.get(fo.getId());
                    int left = ge.getLocation().x;
                    int top = ge.getLocation().y;
                    ImageView iv = ge.getImageView();
                    int visibility = (ge.isVisible()) ? View.VISIBLE : View.INVISIBLE;
                    iv.setVisibility(visibility);
                    int width = 0, height = 0;

                    switch(fo) {
                        case YellowTote1:
                        case YellowTote2:
                        case YellowTote3:
                            width = getResources().getDimensionPixelSize(R.dimen.tote_width);
                            height = getResources().getDimensionPixelSize(R.dimen.tote_height);
                            break;
                        case GreenCan1:
                        case GreenCan2:
                        case GreenCan3:
                        case GreenCan4:
                        case GreenCan5:
                        case GreenCan6:
                        case GreenCan7:
                            width = getResources().getDimensionPixelSize(R.dimen.can_width);
                            height = getResources().getDimensionPixelSize(R.dimen.can_height);
                            break;
                    }

                    RelativeLayout.LayoutParams lp = new RelativeLayout.LayoutParams(
                            width,
                            height
                    );

                    lp.setMargins(left - width/2, top - height/2, 0, 0);
                    lp.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
                    lp.addRule(RelativeLayout.ALIGN_PARENT_TOP);
                    iv.setLayoutParams(lp);
                }
            }
        } else {
            for(AutoFieldObject fo : AutoFieldObject.values()) {
                if (fo.getId() != AutoFieldObject.Robot.getId()) {
                    RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) autoFieldObjects.get(fo.getId()).getImageView().getLayoutParams();
                    autoFieldObjects.get(fo.getId()).setLocation(lp.leftMargin + lp.width/2, lp.topMargin + lp.height/2);
                }
            }
        }
    }

    private GameElement findCollidingElement(View v, int x, int y) {
        Rect viewRect   = new Rect();
        int quarterHeight  = v.getHeight()/4;
        int quarterWidth   = v.getWidth()/4;
        viewRect.left   = x - quarterWidth;
        viewRect.top    = y - quarterHeight;
        viewRect.right  = x + quarterWidth;
        viewRect.bottom = y + quarterHeight;

        Rect r1 = new Rect();
        for(AutoFieldObject fo : AutoFieldObject.values()) {
            if(fo.getId() != v.getId()) {
                GameElement ge = autoFieldObjects.get(fo.getId());
                if(ge.isVisible()) {
                    ge.getImageView().getHitRect(r1);
                    if (Rect.intersects(viewRect, r1)) {
                        return ge;
                    }
                }
            }
        }
        return null;
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onPause() {
        super.onPause();
        saveData();
        tmDBAdapter.close();
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
        GameElement el = autoFieldObjects.get(v.getId());
        if(v.getId()== AutoFieldObject.Robot.getId()) {
            menu.setHeaderIcon(R.drawable.robot_50x50);
            menu.setHeaderTitle("Robot");

            if (lastElementCollided != null) {
                switch (lastElementCollided.getElementType()) {
                    case YELLOW_TOTE:
                        menu.add(0, 0, 0, "Pick Up Tote");
                        if (el.getStackSize() > 1) {
                            menu.add(0, 1, 0, "Stack Tote");
                        }
                        menu.add(0, 3, 0, "Knock Tote Over");
                        break;
                    case CAN:
                        menu.add(0, 4, 0, "Pick Up Can");
                        menu.add(0, 6, 0, "Knock Can Over");
                        break;
                    case TRASH:
                        break;
                    case ROBOT:
                        break;
                    default:
                        Toast.makeText(getBaseContext(), "What was that? A piece of paper?", Toast.LENGTH_LONG).show();
                        break;
                }
            } else {
                GameElement robot = autoFieldObjects.get(AutoFieldObject.Robot.getId());
                if (robot.getStackSize() > 1) {
                    if (robot.nextElement().isTote()) {
                        menu.add(0, 2, 0, "Set tote(s) down");
                    } else if (robot.nextElement().isCan()) {
                        menu.add(0, 5, 0, "Set can down");
                    }
                }
            }
        } else if(autoFieldObjects.get(v.getId()).getElementType() == GameElement.GameElementType.YELLOW_TOTE) {
            menu.setHeaderIcon(R.drawable.yellow_tote_top_down_25x38);
            menu.setHeaderTitle("Yellow Tote");

            menu.add(1,0,0, "Pick Up Tote");
            menu.add(1,1,0, "Stack Tote");
            menu.add(1,2,0, "Knock Tote Over");
        } if(autoFieldObjects.get(v.getId()).getElementType() == GameElement.GameElementType.CAN) {
            menu.setHeaderIcon(R.drawable.green_can_top_down_25x25);
            menu.setHeaderTitle("Green Can");

            menu.add(2,0,0, "Pick Up Can");
            menu.add(2,1,0, "Knock Can Over");
        }
    }

     /*
     * This method is called when an item in a context menu is selected.
     *
     */
    @Override
    public boolean onContextItemSelected(MenuItem item) {
        switch(item.getGroupId()) {
            case 0: // Robot actions
                switch (item.getItemId()) {
                    case 0:
                        pickUp(item.getItemId());
                        break;
                    case 1:
                        stackUp(item.getItemId());
                        break;
                    case 2:
                        setDown(item.getItemId());
                        break;
                    case 3:
                        knockOver(item.getItemId());
                        break;
                    case 4:
                        pickUp(item.getItemId());
                        break;
                    case 5:
                        setDown(item.getItemId());
                        break;
                    case 6:
                        knockOver(item.getItemId());
                }
                break;
            case 1: // Tote actions
                switch (item.getItemId()) {
                    case 0:
                        pickUp(item.getItemId());
                        break;
                    case 1:
                        stackUp(item.getItemId());
                        break;
                    case 2:
                        knockOver(item.getItemId());
                        break;
                }
                break;
            case 2: // Can actions
                switch (item.getItemId()) {
                    case 0:
                        pickUp(item.getItemId());
                        break;
                    case 1:
                        knockOver(item.getItemId());
                        break;
                }
                break;
        }
        return true;
    }

    private void knockOver(int itemId) {
        if(lastViewTouched != null) {
            Toast.makeText(getBaseContext(), "OOPS!", Toast.LENGTH_SHORT).show();
            lastViewTouched = null;
        }
    }

    private void pickUp(int itemId) {
        GameElement robot;
        GameElement object;
        if(lastViewTouched != null) {
            GameElement el = autoFieldObjects.get(lastViewTouched.getId());
            if(el.isRobot()) {
                robot = el;
                object = lastElementCollided;
            } else {
                robot = lastElementCollided;
                object = el;
            }

            object.makeInvisible();
            robot.pushToStack(object);

            lastViewTouched = null;
        }
    }

    public void setDown(int itemId) {
        GameElement robot = autoFieldObjects.get(AutoFieldObject.Robot.getId());
        GameElement currElement;
        for(int i = 0; i < robot.getStackSize(); i++) {
            currElement = robot.popFromStack();
            currElement.makeVisible();
            currElement.setLocation(robot.getLocation().x, robot.getLocation().y);
            setViewLayout(currElement.getImageView(), robot.getLocation().x, robot.getLocation().y);
        }
    }

    private void stackUp(int itemId) {

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
            View view = (View) event.getLocalState();
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
                    lastViewTouched = view;
                    dragging = false;

                    Point finalLocation = null;
                    if(view.getId() == AutoFieldObject.Robot.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.Robot.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.YellowTote1.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.YellowTote1.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.YellowTote2.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.YellowTote2.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.YellowTote3.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.YellowTote3.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.GreenCan1.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.GreenCan1.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.GreenCan2.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.GreenCan2.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.GreenCan3.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.GreenCan3.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.GreenCan4.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.GreenCan4.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.GreenCan5.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.GreenCan5.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.GreenCan6.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.GreenCan6.getId()).getLocation();
                    } else if(view.getId() == AutoFieldObject.GreenCan7.getId()) {
                        finalLocation = autoFieldObjects.get(AutoFieldObject.GreenCan7.getId()).getLocation();
                    }
                    if(finalLocation != null) {
                        finalLocation.set((int) event.getX(), (int) event.getY());
                    }


                    ViewGroup owner = (ViewGroup) view.getParent();
                    RelativeLayout container = (RelativeLayout) v;
                    if(owner.getId() != container.getId()) {
                        owner.removeView(view);
                        container.addView(view);
                    }

                    int left = (int) event.getX();
                    int top = (int) event.getY();
                    autoFieldObjects.get(view.getId()).getLocation().set(left, top);

                    /*
                    if(view.getId() == AutoFieldObject.Robot.getId()) {
                        setInitialRobotLayout();
                    } else {
                        setViewLayout(view, left, top);
                    }
                    */
                    setViewLayout(view, left, top);

                    lastElementCollided = findCollidingElement(view, left, top);

                    // If the robot has something on it, show context
                    // If it's touching a tote, offer to stack
                    // if it's not touching anything, offer to drop if it
                    // If the robot has nothing and is not in contact with something, don't show context
                    // If the lastViewTouched is not a robot, and the lastElementCollided is not the robot, don't show context
                    if(autoFieldObjects.get(lastViewTouched.getId()).isRobot()) {
                        if(lastElementCollided != null) {
                            lastViewTouched.showContextMenu();
                        } else if(autoFieldObjects.get(lastViewTouched.getId()).getStackSize() > 1) {
                            lastViewTouched.showContextMenu();
                        }
                    } else {
                        if(lastElementCollided != null && lastElementCollided.isRobot()) {
                            lastViewTouched.showContextMenu();
                        }
                    }

                    view.setVisibility(View.VISIBLE);
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    //v.setBackgroundDrawable(normalShape);
                case DragEvent.ACTION_DRAG_LOCATION:
                    if(dragging) {
                        if(view != null && view.getId() == AutoFieldObject.Robot.getId()) {
                            Point robotFinalLocation = autoFieldObjects.get(AutoFieldObject.Robot.getId()).getLocation();
                            robotFinalLocation.set((int) event.getX(), (int) event.getY());
                        }
                    }
                    break;
                default:
                    break;
            }
            return true;
        }
    }
}
