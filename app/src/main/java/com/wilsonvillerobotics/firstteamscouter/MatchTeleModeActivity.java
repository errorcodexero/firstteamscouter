package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Point;
import android.os.Bundle;
import android.view.DragEvent;
import android.view.Menu;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewParent;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ALLIANCE_POSITION;
import com.wilsonvillerobotics.firstteamscouter.GaugeLayout.GaugeType;
import com.wilsonvillerobotics.firstteamscouter.GameElement.GameElementType;
import com.wilsonvillerobotics.firstteamscouter.GameElement.GameElementState;
import com.wilsonvillerobotics.firstteamscouter.GameElement.GameElementLocation;

import java.util.ArrayList;
import java.util.HashMap;

public class MatchTeleModeActivity extends Activity {

    protected String defenseA, defenseB, defenseC, defenseD;

    public enum TeleFieldObject {
        Robot(R.id.imgRobot, GameElementType.ROBOT, GameElementState.UPRIGHT, GameElementLocation.GROUND),
        GroundToteUp(R.id.imgGroundToteUp, GameElementType.GRAY_TOTE, GameElementState.UPRIGHT, GameElementLocation.GROUND),
        GroundToteDown(R.id.imgGroundToteDown, GameElementType.GRAY_TOTE, GameElementState.UPSIDEDOWN, GameElementLocation.GROUND),
        GroundToteYellow(R.id.imgGroundToteYellow, GameElementType.YELLOW_TOTE, GameElementState.UPRIGHT, GameElementLocation.GROUND),
        HumanTote(R.id.imgHumanTote, GameElementType.GRAY_TOTE, GameElementState.UPRIGHT, GameElementLocation.FEEDER),
        StepTote(R.id.imgStepTote, GameElementType.GRAY_TOTE, GameElementState.UPRIGHT, GameElementLocation.STEP),
        StepToteYellow(R.id.imgStepTote, GameElementType.YELLOW_TOTE, GameElementState.UPRIGHT, GameElementLocation.STEP),
        CanUp(R.id.imgCanUp, GameElementType.CAN, GameElementState.UPRIGHT, GameElementLocation.GROUND),
        CanSide(R.id.imgCanSide, GameElementType.CAN, GameElementState.ONSIDE, GameElementLocation.GROUND),
        CanDown(R.id.imgCanDown, GameElementType.CAN, GameElementState.UPSIDEDOWN, GameElementLocation.GROUND);

        private int id;
        private GameElementType type;
        private GameElementState state;
        private GameElementLocation location;

        TeleFieldObject(int id, GameElementType et, GameElementState state, GameElementLocation loc) {
            this.id = id;
            this.type = et;
            this.state = state;
            this.location = loc;
        }



        public int getId() {
            return this.id;
        }

        public GameElementType getType() {
            return this.type;
        }

        public GameElementState getState() {
            return this.state;
        }

        public GameElementLocation getLocation() {
            return this.location;
        }
    }

	protected TeamMatchDBAdapter tmDBAdapter;
    protected TeamMatchTransactionDataDBAdapter tmtdDBAdapter;
    protected long teamMatchID;
	protected long teamID;
	protected long matchID;
    protected long competitionID;
    protected int teamNumber;
	protected Button btnSubmit, btnRotateElements;
    private ALLIANCE_POSITION tabletAlliancePosition;
    private int matchNumber;

    private View lastViewTouched;

    protected Boolean fieldOrientationRedOnRight;
    private boolean dataChanged;

    protected ArrayList<Transaction> transactionList;

    protected HashMap<Integer, GameElement> teleFieldObjects;
    protected HashMap<Integer, GaugeLayout> teleGauges;

    protected HashMap<Integer, GaugeType> teleGaugeTypes;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_telemode);

        this.dataChanged = false;
		
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
        //this.tabletAlliancePosition = ALLIANCE_POSITION.getAlliancePositionForString(intent.getStringExtra("tablet_id"));
        //this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        //this.matchNumber = intent.getIntExtra("match_number", 0);
        //this.teamMatchID = intent.getLongExtra("tmID", -1);
        //this.teamNumber  = intent.getIntExtra("team_number", -1);
        //this.competitionID = intent.getLongExtra("competition_id", -1);
        this.defenseA = intent.getStringExtra("DefenseA");
        this.defenseB = intent.getStringExtra("DefenseB");
        this.defenseC = intent.getStringExtra("DefenseC");
        this.defenseD = intent.getStringExtra("DefenseD");

        Toast.makeText(this,this.defenseA,Toast.LENGTH_LONG);
    }

    private void buildIntent(Intent intent) {
        //intent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
        //intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        //intent.putExtra("match_number", matchNumber);
        //intent.putExtra("tmID", teamMatchID);
        //intent.putExtra("team_number", teamNumber);
        //intent.putExtra("competition_id", competitionID);
    }

    private void initFieldObjects() {
        this.teleFieldObjects = new HashMap<Integer, GameElement>();

        for(TeleFieldObject fo : TeleFieldObject.values()) {
            GameElement ge = (GameElement)findViewById(fo.getId());

            if(ge == null) {
                ge = new GameElement(getBaseContext());
            }
            ge.setOnTouchListener(new MyViewTouchListener());
            registerForContextMenu(ge);
            ge.initGameElement(fo.getId(), new Point(), true, fo.getType(), fo.getState(), fo.getLocation());

            this.teleFieldObjects.put(fo.getId(), ge);
        }
    }

    private void initGauges() {
        teleGaugeTypes = new HashMap<Integer, GaugeLayout.GaugeType>();
        teleGaugeTypes.put(R.id.Platform_Gauge_TableView, GaugeType.PLATFORM);
        teleGaugeTypes.put(R.id.Robot_Gauge_TableView, GaugeType.ROBOT);
        teleGaugeTypes.put(R.id.Step_Gauge_TableView, GaugeType.STEP);
        teleGaugeTypes.put(R.id.Floor_Gauge_TableView, GaugeType.GROUND);

        teleGauges = new HashMap<Integer, GaugeLayout>();

        for(int id : teleGaugeTypes.keySet()) {
            GaugeLayout gauge = (GaugeLayout) findViewById(id);
            gauge.init(teleGaugeTypes.get(id), new MyViewDragListener());
            //gauge.setGaugeType(teleGaugeTypes.get(id));
            gauge.setGaugeName(teleGaugeTypes.get(id).getGaugeName());
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
            int action = motionEvent.getAction();
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
                    if(view.getClass() == GameElement.class) {
                        GameElement gameElement = (GameElement)view;
                        ViewParent parent = view.getParent();
                        ViewParent grandparent = (parent != null) ? parent.getParent() : null;
                        if(grandparent != null && grandparent.getClass() == GaugeLayout.class) {
                            GaugeLayout gl = (GaugeLayout)grandparent;
                            GaugeRow gr    = (GaugeRow)parent;

                            TransportContainer tp = buildTransportContainer(gl, gr);

                            ClipData.Item item = new ClipData.Item(String.valueOf(tp.getNumElements()));
                            String[] mimeType  = {"text/plain"};
                            ClipData clipData  = new ClipData(String.valueOf(tp.getNumElements()),mimeType,item);

                            View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(tp);
                            tp.startDrag(clipData, shadowBuilder, tp, 0);
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
                        } else if(grandparent != null && grandparent.getClass() == RelativeLayout.class) {
                            RelativeLayout rl = (RelativeLayout)grandparent;
                            TransportContainer tp = buildTransportContainer(rl, gameElement);
                            // TODO - Fix this - currently puts transport container image in upper left of screen until it's dropped on gauge
                            // but it's left there if the tp is not dropped on a gauge
                            /*
                            GameElement ge = new GameElement(rl.getContext());
                            ge.setImageDrawable(gameElement.getDrawable());
                            ge.setElementType(gameElement.getElementType());
                            ge.setElementState(gameElement.getElementState());
                            */

                            ClipData.Item item = new ClipData.Item(String.valueOf(tp.getNumElements()));
                            String[] mimeType  = {"text/plain"};
                            ClipData clipData  = new ClipData(String.valueOf(tp.getNumElements()),mimeType,item);

                            View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(tp);
                            tp.startDrag(clipData, shadowBuilder, tp, 0);
                            return true;

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

    private TransportContainer buildTransportContainer(GaugeLayout gl, GaugeRow gr) {
        TransportContainer tp = new TransportContainer(gl.getContext());
        tp.setVisibility(View.VISIBLE);
        tp.setOrientation(LinearLayout.VERTICAL);
        tp.setStartLocation(gl.getElementLocation());
        gl.addView(tp);

        ArrayList<GaugeRow> activeRowsAbove = gl.getActiveRowsAbove(gr);
        String log = "Active Row Above Count: " + activeRowsAbove.size() + "\n";
        for(int i = activeRowsAbove.size() - 1; i >= 0; i--) {
            GaugeRow gRow = activeRowsAbove.get(i);
            log += "Received row at index " + gRow.getRowIndex() + "\n";

            GameElement ge = new GameElement(gl.getContext());
            ge.setImageDrawable(gRow.getGameElement().getDrawable());
            ge.setElementType(gRow.getGameElement().getElementType());
            ge.setElementState(gRow.getGameElement().getElementState());
            ge.setElementLocation(gl.getElementLocation());
            gRow.deactivate(GameElementType.GRAY_TOTE, GameElementState.UPRIGHT, new MyViewDragListener());
            tp.addGameElement(ge);
        }
        FTSUtilities.printToConsole(log);

        tp.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
        final int size=View.MeasureSpec.makeMeasureSpec(0,View.MeasureSpec.UNSPECIFIED);
        tp.measure(size, size);
        tp.layout(0, 0, tp.getMeasuredWidth(), tp.getMeasuredHeight());
        tp.invalidate();

        return tp;
    }

    private TransportContainer buildTransportContainer(RelativeLayout rl, GameElement ge) {
        TransportContainer tp = new TransportContainer(rl.getContext());
        tp.setVisibility(View.VISIBLE);
        tp.setOrientation(LinearLayout.VERTICAL);
        rl.addView(tp);

        //GameElement gameElement = new GameElement(rl.getContext(), ge);

        //final android.view.ViewParent parent = gameElement.getParent ();
        //if (parent instanceof android.view.ViewManager)
        //{
        //    final android.view.ViewManager viewManager = (android.view.ViewManager) parent;
        //    viewManager.removeView(gameElement);
        //}

        GameElement gameElement = new GameElement(tp.getContext());
        gameElement.setImageDrawable(ge.getDrawable());
        gameElement.setElementType(ge.getElementType());
        gameElement.setElementState(ge.getElementState());
        gameElement.setElementLocation(ge.getElementLocation());
        tp.addGameElement(gameElement);
        tp.setStartLocation(gameElement.getElementLocation());

        tp.setLayoutParams(new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.WRAP_CONTENT, RelativeLayout.LayoutParams.WRAP_CONTENT));
        final int size=View.MeasureSpec.makeMeasureSpec(0,View.MeasureSpec.UNSPECIFIED);
        tp.measure(size, size);
        tp.layout(0, 0, tp.getMeasuredWidth(), tp.getMeasuredHeight());
        tp.invalidate();

        return tp;
    }

    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("MatchTeleModeActivity::openDatabase : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this);
            tmtdDBAdapter = new TeamMatchTransactionDataDBAdapter(this);

            Cursor c = tmDBAdapter.getEntry(this.teamMatchID);
            if(c.moveToFirst()) {
                this.teamID = c.getLong(c.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID));
                this.matchID = c.getLong(c.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID));
            }
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
            tmtdDBAdapter = null;
        }
    }

    private void saveData() {
        if(dataChanged) {
            for (Transaction t : transactionList) {
                t.setReadyToExport(true);
                HashMap<String, Object> values = t.getValuesHashMap();
                long id = tmtdDBAdapter.createTeamMatchDataTransaction(values);
            }
        }
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        if(tmDBAdapter == null) tmDBAdapter = new TeamMatchDBAdapter(this);
        if(tmtdDBAdapter == null) tmtdDBAdapter = new TeamMatchTransactionDataDBAdapter(this);
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
        FTSUtilities.printToConsole("SelectTeamMatchActivity::onStop : CLOSING DB\n");
        saveData();
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
                        int count = (view.getClass() == TransportContainer.class) ? ((TransportContainer)view).getChildCount() : 1;
                        gl.highlightRows((GaugeRow)v, count);
                    }
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    if(v.getClass() == GaugeRow.class) {
                        GaugeLayout gl = (GaugeLayout)v.getParent();
                        int count = (view.getClass() == TransportContainer.class) ? ((TransportContainer)view).getChildCount() : 1;
                        gl.unHighlightRows((GaugeRow)v, count);
                    }
                    break;
                case DragEvent.ACTION_DROP:
                    dataChanged = true;
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

                    if(v.getClass() == GaugeRow.class) {
                        GaugeRow gr = (GaugeRow)v;
                        gr.setOnDragListener(null);
                        GameElement ge = gr.getGameElement();

                        if(ge != null) {
                            GaugeLayout gaugeLayout = (GaugeLayout)gr.getParent();
                            if(view.getClass() == GameElement.class) {
                                GameElement ge2 = (GameElement)view;
                                // TODO - Fix this to use a TransportContainer so I know what type of element this represents
                                String from = ge2.getElementLocation().getLocation(); //"Floor";
                                String to = gaugeLayout.getGaugeName();
                                gr.activate(ge2.getElementType(), ge2.getElementState(), ge2.getDrawable(), new MyViewTouchListener());
                                recordTransaction("Place", from, to, ge2, gr);

                                if(gaugeLayout.getGaugeType() == GaugeType.ROBOT) {

                                    this.resetNonRobotGauges();
                                }
                            } else if(view.getClass() == TransportContainer.class) {
                                TransportContainer transportContainer = (TransportContainer)view;
                                FTSUtilities.printToConsole("DROP on Gauge: " + gaugeLayout.getGaugeName());
                                String from = transportContainer.getStartLocation().getLocation();// ((GaugeLayout)transportContainer.getParent()).getGaugeName();
                                String toGauge = gaugeLayout.getGaugeName();

                                gaugeLayout.activateRows(gr, transportContainer, new MyViewTouchListener());
                                recordTransaction("Place", from, toGauge, transportContainer, gr);
                                if(gaugeLayout.getGaugeType() == GaugeType.ROBOT) {
                                    this.resetNonRobotGauges();
                                }
                                // ditch the linear layout so garbage collection can do its job
                                ViewParent par = transportContainer.getParent();
                                if(par.getClass() == GaugeLayout.class) {
                                    ((GaugeLayout) transportContainer.getParent()).removeView(transportContainer);
                                } else if(par.getClass() == RelativeLayout.class) {
                                    ((RelativeLayout) transportContainer.getParent()).removeView(transportContainer);
                                } else if (par instanceof android.view.ViewManager) {
                                    final android.view.ViewManager viewManager = (android.view.ViewManager)par;
                                    viewManager.removeView(transportContainer);
                                }
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

    private void recordTransaction(String action, String from, String to, GameElement gameElement, GaugeRow gr) {
        Transaction t = new Transaction();

        t.setTeamID(teamID);
        t.setMatchID(matchID);
        t.setTimestamp(System.nanoTime());
        t.setAction(action);
        t.setActionPhase("Tele");
        t.setActionStartLocationName(from);
        t.setActionEndLocationName(to);

        int x = (int)gameElement.getX();
        int y = (int)gameElement.getY();
        Point p = new Point(x, y);
        t.setActionStart(p);

        x = (int)gr.getX();
        y = (int)gr.getY();
        p = new Point(x, y);
        t.setActionEnd(p);

        String gameElementTypes[] = new String[]{GameElementType.GRAY_TOTE.getType()};
        String gameElementStates[] = new String[]{GameElementState.UPRIGHT.getState()};
        t.setElementTypes(gameElementTypes);
        t.setElementStates(gameElementStates);

        transactionList.add(t);
    }

    private void recordTransaction(String action, String from, String to, TransportContainer container, GaugeRow gr) {
        Transaction t = new Transaction();

        t.setTeamID(teamID);
        t.setMatchID(matchID);
        t.setTimestamp(System.nanoTime());

        t.setAction(action);
        t.setActionPhase("Tele");
        t.setActionStartLocationName(from);
        t.setActionEndLocationName(to);

        int x = (int)container.getX();
        int y = (int)container.getY();
        Point p = new Point(x, y);
        t.setActionStart(p);

        x = (int)gr.getX();
        y = (int)gr.getY();
        p = new Point(x, y);
        t.setActionEnd(p);

        ArrayList<String> gameElementTypes = new ArrayList<String>();
        ArrayList<String> gameElementStates = new ArrayList<String>();

        for(GameElement ge : container.getGameElements()) {
            gameElementTypes.add(ge.getElementType().getType());
            gameElementStates.add(ge.getElementState().getState());
        }

        t.setElementTypes(gameElementTypes.toArray(new String[gameElementTypes.size()]));
        t.setElementStates(gameElementStates.toArray(new String[gameElementStates.size()]));

        transactionList.add(t);
    }
}
