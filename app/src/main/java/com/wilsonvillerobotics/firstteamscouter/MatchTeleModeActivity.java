package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Context;
import android.content.Intent;
import android.database.SQLException;
import android.graphics.Point;
import android.os.Bundle;
import android.support.v4.view.MotionEventCompat;
import android.view.DragEvent;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.LinearLayout;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionsDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ALLIANCE_POSITION;
import com.wilsonvillerobotics.firstteamscouter.GaugeLayout.GaugeType;
import com.wilsonvillerobotics.firstteamscouter.GameElement.GameElementType;
import com.wilsonvillerobotics.firstteamscouter.GameElement.GameElementState;

import java.util.ArrayList;
import java.util.HashMap;

public class MatchTeleModeActivity extends Activity {

    public enum TeleFieldObject {
        Robot(R.id.imgRobot, GameElement.GameElementType.ROBOT, GameElement.GameElementState.UPRIGHT, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_VISIBLE),
        GroundToteUp(R.id.imgGroundToteUp, GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE1_VISIBLE),
        GroundToteDown(R.id.imgGroundToteDown, GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPSIDEDOWN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE2_VISIBLE),
        GroundToteYellow(R.id.imgGroundToteYellow, GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, "", "", ""),
        HumanTote(R.id.imgHumanTote, GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE3_VISIBLE),
        StepTote(R.id.imgStepTote, GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN1_VISIBLE),
        CanUp(R.id.imgCanUp, GameElement.GameElementType.CAN, GameElement.GameElementState.UPRIGHT, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN2_VISIBLE),
        CanSide(R.id.imgCanSide, GameElement.GameElementType.CAN, GameElement.GameElementState.ONSIDE, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN3_VISIBLE),
        CanDown(R.id.imgCanDown, GameElement.GameElementType.CAN, GameElement.GameElementState.UPSIDEDOWN, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_X, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_Y, TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN4_VISIBLE);

        private int id;
        private String dbColumnX, dbColumnY, dbVisible;
        private GameElement.GameElementType type;
        private GameElement.GameElementState state;

        TeleFieldObject(int id, GameElement.GameElementType et, GameElement.GameElementState state, String colX, String colY, String visible) {
            this.id = id;
            this.dbColumnX = colX;
            this.dbColumnY = colY;
            this.dbVisible = visible;
            this.type = et;
            this.state = state;
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

        public GameElement.GameElementState getState() {
            return this.state;
        }
    }

	protected TeamMatchDBAdapter tmDBAdapter;
    protected TeamMatchTransactionDataDBAdapter tmtdDBAdapter;
    protected TeamMatchTransactionsDBAdapter tmtDBAdapter;
    protected long teamMatchID;
	protected long teamID;
	protected long matchID;
    protected String teamNumber;
	protected Button btnSubmit, btnRotateElements;
    private ALLIANCE_POSITION tabletAlliancePosition;
    private int matchNumber;

    private View lastViewTouched;

	protected Boolean fieldOrientationRedOnRight;

    protected ArrayList<Transaction> transactionList;

    protected HashMap<Integer, GameElement> teleFieldObjects;
    protected HashMap<Integer, GaugeLayout> teleGauges;

    protected HashMap<Integer, GaugeType> teleGaugeTypes;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_telemode);
		
		this.processIntent(getIntent());

        this.initFieldObjects();
        this.initGauges();

        this.transactionList = new ArrayList<Transaction>();

        teamID = -1;
		matchID = -1;

        this.openDatabase();
        this.configButtons();
	}

    private void processIntent(Intent intent) {
        this.tabletAlliancePosition = ALLIANCE_POSITION.getAlliancePositionForString(intent.getStringExtra("tablet_id"));
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
        this.teamNumber  = intent.getStringExtra("team_number");
    }

    private void buildIntent(Intent intent) {
        intent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
        intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        intent.putExtra("match_number", matchNumber);
        intent.putExtra("tmID", teamMatchID);
        intent.putExtra("team_number", teamNumber);
    }

    private void initFieldObjects() {
        this.teleFieldObjects = new HashMap<Integer, GameElement>();

        for(TeleFieldObject fo : TeleFieldObject.values()) {
            ImageView iv = (ImageView)findViewById(fo.getId());
            if(iv == null) {
                iv = new ImageView(getBaseContext());
            }
            iv.setOnTouchListener(new MyViewTouchListener());
            registerForContextMenu(iv);
            //ge.setImageView(iv);
            GameElement ge = new GameElement(
                    fo.getId(),
                    iv,
                    new Point(),
                    true,
                    fo.getType(),
                    fo.getState()
            );
            //ge.makeVisible();

            this.teleFieldObjects.put(fo.getId(), ge);
        }
    }

    private void initGauges() {
        teleGaugeTypes = new HashMap<Integer, GaugeLayout.GaugeType>();
        teleGaugeTypes.put(R.id.Platform_Gauge_TableView, GaugeType.PLATFORM);
        teleGaugeTypes.put(R.id.Robot_Gauge_TableView, GaugeType.ROBOT);
        teleGaugeTypes.put(R.id.Step_Gauge_TableView, GaugeType.STEP);
        teleGaugeTypes.put(R.id.Floor_Gauge_TableView, GaugeType.FLOOR);

        teleGauges = new HashMap<Integer, GaugeLayout>();

        for(int id : teleGaugeTypes.keySet()) {
            GaugeLayout gauge = (GaugeLayout) findViewById(id);
            gauge.init(new MyViewDragListener());
            gauge.setGaugeType(teleGaugeTypes.get(id));
            gauge.setGaugeName(teleGaugeTypes.get(id).getGaugeTypeString());
            teleGauges.put(id, gauge);
        }
    }

    private void configButtons() {
        btnSubmit = (Button) findViewById(R.id.btnSubmitMatchTele);
        btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                Intent autoIntent = new Intent(v.getContext(), MatchNotesActivity.class);
                buildIntent(autoIntent);
                startActivity(autoIntent);
            }
        });

        btnRotateElements = (Button)findViewById(R.id.btnRotateElements);
        btnRotateElements.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

            }
        });
    }

    private final class MyViewTouchListener implements View.OnTouchListener {
        @Override
        public boolean onTouch(View view, MotionEvent motionEvent) {
            int action = MotionEventCompat.getActionMasked(motionEvent);
            switch(action) {
                case MotionEvent.ACTION_POINTER_DOWN:
                    return true;
                case MotionEvent.ACTION_UP:
                    return true;
                case MotionEvent.ACTION_POINTER_UP:
                    return true;
                case MotionEvent.ACTION_MOVE:
                    //view.setVisibility(View.GONE);
                    return true;
                case MotionEvent.ACTION_OUTSIDE:
                    return true;
                case MotionEvent.ACTION_CANCEL:
                    return true;
                case MotionEvent.ACTION_DOWN:
                    /*
                     * If it's a Gauge, need to determine which gauge and which row (hit detection)
                     * If a single touch event, move that row only
                     * If part of a multi-touch event, find second touch, find row (hit detection)
                     *    and remove a stack equal to the number of rows spanned. If not enough
                     *    totes are on the stack, remove as many as are between the two rows, inclusive
                     *
                     * Come up with a touch/multi-touch state machine for this
                     */
                    if(view.getClass() == ImageView.class) {
                        ViewParent parent = view.getParent();
                        ViewParent grandparent = (parent != null) ? parent.getParent() : null;
                        if(grandparent != null && grandparent.getClass() == GaugeLayout.class) {
                            GaugeLayout gl = (GaugeLayout)grandparent;
                            GaugeRow gr    = (GaugeRow)parent;
                            int rowNum = gr.getRowIndex();

                            LayoutInflater inflater = (LayoutInflater)getApplicationContext().getSystemService
                                    (Context.LAYOUT_INFLATER_SERVICE);
                            LinearLayout llTote = new LinearLayout(gl.getContext()); // (LinearLayout)inflater.inflate(R.layout.layout_single_tote, null);
                            llTote.setVisibility(View.VISIBLE);
                            llTote.setOrientation(LinearLayout.VERTICAL);
                            gl.addView(llTote);

                            int numElements = 0;
                            ArrayList<GaugeRow> activeRowsAbove = gl.getActiveRowsAbove(gr);
                            String log = "Active Row Above Count: " + activeRowsAbove.size() + "\n";
                            for(int i = activeRowsAbove.size() - 1; i >= 0; i--) {
                                GaugeRow gRow = activeRowsAbove.get(i);
                                log += "Received row at index " + gRow.getRowIndex() + "\n";
                                ImageView ivElement = new ImageView(gl.getContext());
                                ivElement.setImageDrawable(gRow.getImageView().getDrawable());
                                gRow.deactivate(GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, new MyViewDragListener());
                                llTote.addView(ivElement);
                            }
                            FTSUtilities.printToConsole(log);

                            ClipData.Item item = new ClipData.Item(String.valueOf(numElements));
                            String[] mimeType  = {"text/plain"};
                            ClipData clipData  = new ClipData(String.valueOf(numElements),mimeType,item);

                            llTote.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
                            final int size=View.MeasureSpec.makeMeasureSpec(0,View.MeasureSpec.UNSPECIFIED);
                            llTote.measure(size, size);
                            llTote.layout(0, 0, llTote.getMeasuredWidth(), llTote.getMeasuredHeight());
                            llTote.invalidate();
                            View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(llTote);
                            llTote.startDrag(clipData, shadowBuilder, llTote, 0);
                            return true;
                        /*
                        http://www.programcreek.com/java-api-examples/index.php?api=android.view.View.DragShadowBuilder
                        TextView shadowView=(TextView)View.inflate(mTextView.getContext(),com.android.internal.R.layout.text_drag_thumbnail,null);
                          if (shadowView == null) {
                            throw new IllegalArgumentException("Unable to inflate text drag thumbnail");
                          }
                          shadowView.setLayoutParams(new LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT,ViewGroup.LayoutParams.WRAP_CONTENT));
                          final int size=View.MeasureSpec.makeMeasureSpec(0,View.MeasureSpec.UNSPECIFIED);
                          shadowView.measure(size,size);
                          shadowView.layout(0,0,shadowView.getMeasuredWidth(),shadowView.getMeasuredHeight());
                          shadowView.invalidate();
                          return new DragShadowBuilder(shadowView);
                         */
                        }
                    }
                    ClipData data = ClipData.newPlainText("", "");
                    View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(view);
                    view.startDrag(data, shadowBuilder, view, 0);
                    return true;
            }
            return false;
        }
    }

    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("MatchTeleModeActivity::openDatabase : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this).open();
            tmtdDBAdapter = new TeamMatchTransactionDataDBAdapter(this).open();
            tmtDBAdapter = new TeamMatchTransactionsDBAdapter(this).open();
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
            tmtdDBAdapter = null;
            tmtDBAdapter = null;
        }
    }

    private void saveData() {
        for(Transaction t : transactionList) {
            HashMap<String, Object> values = t.getValuesHashMap();
            long id = tmtdDBAdapter.createTeamMatchTransaction(values);
            if(id != -1) tmtDBAdapter.createTeamMatchTransaction(teamMatchID, id);
        }
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        if(tmDBAdapter == null) {
        	tmDBAdapter = new TeamMatchDBAdapter(this);
            tmtdDBAdapter = new TeamMatchTransactionDataDBAdapter(this);
            tmtDBAdapter = new TeamMatchTransactionsDBAdapter(this);
        }
        tmDBAdapter.open();
        tmtdDBAdapter.open();
        tmtDBAdapter.open();
    }

    @Override
    protected void onStart() {
        super.onStart();
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
        saveData();
        FTSUtilities.printToConsole("SelectTeamMatchActivity::onStop : CLOSING DB\n");
		tmDBAdapter.close();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.enter_data, menu);
		return true;
	}

    public class MyViewDragListener implements View.OnDragListener {
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
                    if(v.getClass() == GaugeRow.class) {
                        GaugeLayout gl = (GaugeLayout)v.getParent();
                        int count = (view.getClass() == LinearLayout.class) ? ((LinearLayout)view).getChildCount() : 1;
                        gl.highlightRows((GaugeRow)v, count);
                    }
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    if(v.getClass() == GaugeRow.class) {
                        GaugeLayout gl = (GaugeLayout)v.getParent();
                        int count = (view.getClass() == LinearLayout.class) ? ((LinearLayout)view).getChildCount() : 1;
                        gl.unHighlightRows((GaugeRow)v, count);
                    }
                    break;
                case DragEvent.ACTION_DROP:
                    lastViewTouched = view;
                    dragging = false;
                    int numRows = 0;

                    ClipData clip = event.getClipData();
                    if (clip != null) {
                        ClipData.Item item = clip.getItemAt(0);
                        if(!item.getText().equals("")) {
                            numRows = Integer.parseInt(item.getText().toString());
                        }
                    }

                    ViewGroup owner = (ViewGroup) v.getParent();

                    if(v.getClass() == GaugeRow.class) {
                        GaugeRow gr = (GaugeRow)v;
                        gr.setOnDragListener(null);
                        ImageView iv = gr.getImageView();

                        if(iv != null) {
                            GaugeLayout gaugeLayout = (GaugeLayout)gr.getParent();
                            if(view.getClass() == ImageView.class) {
                                gr.activate(GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, ((ImageView) view).getDrawable(), new MyViewTouchListener());
                                Transaction t = new Transaction();

                                t.setTeamID(teamID);
                                t.setMatchID(matchID);
                                t.setTimestamp(System.nanoTime());
                                t.setAction("Place");

                                int x = (int)view.getX();
                                int y = (int)view.getY();
                                Point p = new Point(x, y);
                                t.setActionStart(p);

                                x = (int)gr.getX();
                                y = (int)gr.getY();
                                p = new Point(x, y);
                                t.setActionEnd(p);

                                GameElementType gameElementTypes[] = new GameElementType[]{GameElement.GameElementType.GRAY_TOTE};
                                GameElementState gameElementStates[] = new GameElement.GameElementState[]{GameElement.GameElementState.UPRIGHT};
                                t.setElementTypes(gameElementTypes);
                                t.setElementStates(gameElementStates);

                                int gameElementQuantities[] = new int[]{1};
                                t.setElementQuantities(gameElementQuantities);

                                p = new Point(x, y);
                                Point gameElementStartPoints[] = new Point[]{p};
                                Point gameElementEndPoints[] = new Point[]{p};
                                t.setElementStartLocations(gameElementStartPoints);
                                t.setElementEndLocations(gameElementEndPoints);

                                transactionList.add(t);

                                if(gaugeLayout.getGaugeType() == GaugeType.ROBOT) {
                                    this.resetNonRobotGauges();
                                }
                            } else if(view.getClass() == LinearLayout.class) {
                                LinearLayout llTotes = (LinearLayout)view;
                                if(owner.getClass() == GaugeLayout.class) {
                                    GaugeLayout gl = (GaugeLayout)owner;
                                    FTSUtilities.printToConsole("DROP on Gauge: " + gl.getGaugeName());
                                    // Sub class LinearLayout as TransportContainer, then give it functions to work with ArrayLists of GaugeRows.
                                    gl.activateRows(gr, llTotes, new MyViewTouchListener());
                                }
                                if(gaugeLayout.getGaugeType() == GaugeType.ROBOT) {
                                    this.resetNonRobotGauges();
                                }
                                // ditch the linear layout so garbage collection can do its job
                                ((GaugeLayout)llTotes.getParent()).removeView(llTotes);
                            }
                        }
                    }
                    view.setVisibility(View.VISIBLE);
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    break;
                case DragEvent.ACTION_DRAG_LOCATION:
                    break;
                default:
                    break;
            }
            return true;
        }

        private void resetNonRobotGauges() {
            for(GaugeLayout gl : teleGauges.values()) {
                if(gl.getGaugeType() != GaugeType.ROBOT) {
                    gl.deactivateAllRows(new MyViewDragListener());
                }
            }
        }
    }
}
