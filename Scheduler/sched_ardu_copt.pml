
//-----------------------------------------------------------------
// Ardupilot copter sched model in Promela, in progress
// (c) Sergey Staroletov
//-----------------------------------------------------------------

// tasks params are based on 
// https://github.com/ArduPilot/ardupilot/blob/ArduCopter-3.2.1/ArduCopter/ArduCopter.pde#L777
mtype = {
    rc_loop,
    throttle_loop,
    update_GPS,
    update_batt_compass,
    read_aux_switches,
    arm_motors_check,
    auto_trim,
    update_altitude,
    run_nav_updates,
    update_thr_cruise,
    three_hz_loop,
    compass_accumulate,
    barometer_accumulate,
    update_notify,
    one_hz_loop,
    ekf_dcm_check,
    crash_check,
    gcs_check_input,
    gcs_send_heartbeat,
    gcs_send_deferred,
    gcs_data_stream_send,
    update_mount,
    ten_hz_logging_loop,
    fifty_hz_logging_loop,
    read_receiver_rssi,
    telemetry_send,
    userhook_fastLoop,
    userhook_50hz,
    userhook_mediumLoop,
    userhook_slowLoop,
    userhook_superSlowLoop,
};

typedef Task {
    mtype task_id;
    short interval;
    short max_time;
}

#define num_tasks 31
#define MAIN_LOOP_MICROS 10000
#define max_sim 100000 

Task task[num_tasks];


inline setup_tasks() {
    task[0].task_id = rc_loop;	
    task[0].interval = 4;	
    task[0].max_time = 10;

    task[1].task_id = throttle_loop;
    task[1].interval = 8;	
    task[1].max_time = 45;

    task[2].task_id = update_GPS;
    task[2].interval = 8;	
    task[2].max_time = 90;

    task[3].task_id = update_batt_compass;	
    task[3].interval = 40;
    task[3].max_time = 72;

    task[4].task_id = read_aux_switches;	
    task[4].interval = 40;
    task[4].max_time = 5;

    task[5].task_id = arm_motors_check;	
    task[5].interval = 40;	
    task[5].max_time = 1;

    task[6].task_id = auto_trim;	
    task[6].interval = 40;	
    task[6].max_time = 14;

    task[7].task_id = update_altitude;	
    task[7].interval = 40;	
    task[7].max_time = 100;

    task[8].task_id = run_nav_updates;	
    task[8].interval = 8;
    task[8].max_time = 80;

    task[9].task_id = update_thr_cruise;	
    task[9].interval = 40;	
    task[9].max_time = 10;

    task[10].task_id = three_hz_loop;	
    task[10].interval = 133;	
    task[10].max_time = 9;

    task[11].task_id = compass_accumulate;	
    task[11].interval = 8;	
    task[11].max_time = 42;

    task[12].task_id = barometer_accumulate;	
    task[12].interval = 8;	
    task[12].max_time = 25;

    task[13].task_id = update_notify;	
    task[13].interval = 8;	
    task[13].max_time = 10;

    task[14].task_id = one_hz_loop;	
    task[14].interval = 400;	
    task[14].max_time = 42;

    task[15].task_id = ekf_dcm_check;	
    task[15].interval = 40;	
    task[15].max_time = 2;

    task[16].task_id = crash_check;	
    task[16].interval = 40;	
    task[16].max_time = 2;

    task[17].task_id = gcs_check_input;	
    task[17].interval = 8;	
    task[17].max_time = 550;

    task[18].task_id = gcs_send_heartbeat;	
    task[18].interval = 400;	
    task[18].max_time = 150;

    task[19].task_id = gcs_send_deferred;	
    task[19].interval = 8;	
    task[19].max_time = 720;

    task[20].task_id = gcs_data_stream_send;	
    task[20].interval = 8;	
    task[20].max_time = 950;

    task[21].task_id = update_mount;	
    task[21].interval = 8;	
    task[21].max_time = 45;

    task[22].task_id = ten_hz_logging_loop;	
    task[22].interval = 40;	
    task[22].max_time = 30;

    task[23].task_id = fifty_hz_logging_loop;	
    task[23].interval = 8;	
    task[23].max_time = 22;

    task[24].task_id = read_receiver_rssi;	
    task[24].interval = 40;	
    task[24].max_time = 5;

    task[25].task_id = telemetry_send;	
    task[25].interval = 80;	
    task[25].max_time = 10;

    task[26].task_id = userhook_fastLoop;	
    task[26].interval = 4;	
    task[26].max_time = 10;

    task[27].task_id = userhook_50hz;	
    task[27].interval = 8;	
    task[27].max_time = 10;

    task[28].task_id = userhook_mediumLoop;	
    task[28].interval = 40;	
    task[28].max_time = 10;

    task[29].task_id = userhook_slowLoop;	
    task[29].interval = 120;	
    task[29].max_time = 10;

    task[30].task_id = userhook_superSlowLoop;	
    task[30].interval = 400;	
    task[30].max_time = 10;
}


inline print_task_by_id (id) {
    if
        ::rc_loop -> printf("rc_loop");
        ::throttle_loop -> printf("throttle_loop");
        ::update_GPS -> printf("update_GPS");
        ::update_batt_compass  -> printf("update_batt_compass");
        ::read_aux_switches  -> printf("read_aux_switches");
        ::arm_motors_check -> printf("arm_motors_check");
        ::auto_trim  -> printf(" auto_trim");
        ::update_altitude  -> printf("update_altitude");
        ::run_nav_updates -> printf("run_nav_updates");
        ::update_thr_cruise -> printf("update_thr_cruise");
        ::three_hz_loop -> printf("three_hz_loop");
        ::compass_accumulate -> printf("compass_accumulate");
        ::barometer_accumulate -> printf("barometer_accumulate");
        ::update_notify -> printf("update_notify");
        ::one_hz_loop -> printf("one_hz_loop");
        ::ekf_dcm_check -> printf("ekf_dcm_check");
        ::crash_check -> printf("crash_check");
        ::gcs_check_input -> printf("gcs_check_input");
        ::gcs_send_heartbeat -> printf("gcs_send_heartbeat");
        ::gcs_send_deferred -> printf("gcs_send_deferred");
        ::gcs_data_stream_send -> printf("gcs_data_stream_send");
        ::update_mount -> printf("update_mount");
        ::ten_hz_logging_loop -> printf("ten_hz_logging_loop");
        ::fifty_hz_logging_loop -> printf("fifty_hz_logging_loop");
        ::read_receiver_rssi -> printf("read_receiver_rssi");
        ::telemetry_send -> printf("telemetry_send");
        ::userhook_fastLoop -> printf("userhook_fastLoop");
        ::userhook_50hz -> printf("userhook_50hz");
        ::userhook_mediumLoop -> printf("userhook_mediumLoop");
        ::userhook_slowLoop -> printf("userhook_slowLoop");
        ::userhook_superSlowLoop -> printf("userhook_superSlowLoop");
        ::else -> skip
    fi
}

// http://spinroot.com/spin/Man/rand.html
inline rnd (max, ret) {
    int r = 0;
    do
        ::(r < max) -> r++;
        ::(r > 0) -> r--; 
        ::(r > 0) -> break; //non-deterministic exit
    od
    ret = r;
}


int micros = 0;
int spare_ticks = 0;
int spare_micros = 0;
int last_run[num_tasks];
int time_available = 1000;
int tick_counter = 0;


int overruns = 0;
int slipped = 0;
int current_task = 0;


inline task_func(id) {
    int my_max_time_to_run = task[i].max_time;
    int t = 0;
    rnd(my_max_time_to_run, t);
    printf("Running task ");
    print_task_by_id(id);
    printf(" run time = %d\n", t);
    micros = micros + t;
}



//code is based on void AP_Scheduler::run(uint16_t time_available)
inline run_tick(time_available) {
    int run_started_usec  = micros;
    int now = run_started_usec;
    short i = 0;
    do
        ::(i < num_tasks) -> {
            int dt = tick_counter - last_run[i];//?
            int interval_ticks = task[i].interval;
            if
                ::(dt > interval_ticks) -> {
                    int task_time_allowed = task[i].max_time;
                    if
                        ::(dt >= interval_ticks * 2) -> {
                            printf("We've slipped a whole run of this task %d ", i);
                            print_task_by_id(task[i]);
                            printf("!\n");
                            slipped++;
                        } 
                        ::else -> skip;
                    fi
                    
                    if
                        ::(task_time_allowed <= time_available) -> {
                            int task_time_started = now;
                            current_task = i;
                            task_func(i);
                            current_task = -1;

                            last_run[i] = tick_counter;
                            now = micros;
                            int time_taken = now - task_time_started;
                            if
                                ::(time_taken > task_time_allowed) -> {
                                    printf("Scheduler overrun task %d ", i);
                                    print_task_by_id(task[i]);
                                    printf("!\n");
                                    overruns++;
                                }
                                ::else -> skip;  
                            fi
                            if
                                ::(time_taken > time_available) -> break;
                                ::else -> time_available = time_available - time_taken;
                            fi
                        }
                        ::else -> skip
                    fi
                }
                ::else -> skip  
            fi
            i++;
        } 
        ::else -> break
    od

    spare_ticks++; 
    if 
        ::(spare_ticks == 32) -> {
            spare_ticks = spare_ticks / 2;
            spare_micros = spare_micros / 2;
        }
        ::else -> skip
    fi
}

inline loop() {
    int timer = micros;
    //fast loop -?
    tick_counter++;
    int time_available = (timer + MAIN_LOOP_MICROS) - micros;
    run_tick(time_available);
}

active proctype main() {
    setup_tasks();
    //todo: run loop for some bounded time
    do 
        ::(micros < max_sim) -> loop();
        ::else -> break
    od
}


ltl all_fine {[] (overruns == 0 && slipped == 0)}