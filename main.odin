package main

import "core:fmt"
import "core:math"
import "core:mem"
import "core:os"
import "core:runtime"
import "core:slice"
import "core:time"

import rl "vendor:raylib"

// Midi Input:

  // TODO (hitch) 2023-07-31 Look into vendor:portmidi for cross platform midi input...

  when ODIN_OS == .Windows {

    import w32 "core:sys/windows"

    foreign import lib "system:winmm.lib"

    // Types:
      HMIDIIN :: distinct w32.HANDLE

      MM_Result :: enum u32 {
        Success,            /* no error */
        Error,              /* unspecified error */
        Bad_Device,         /* device ID out of range */
        Not_Enabled,        /* driver failed enable */
        Allocated,          /* device already allocated */
        Invalid_Handle,     /* device handle is invalid */
        No_Driver,          /* no device driver present */
        No_Mem,             /* memory allocation error */
        Not_Supported,      /* function isn't supported */
        Bad_Error_Num,      /* error value out of range */
        Invalid_Flag,       /* invalid flag passed */
        Invalid_Param,      /* invalid parameter passed */
        Handle_Busy,        /* handle being used simultaneously on another thread (eg callback) */
        Invalid_Alias,      /* specified alias not found */
        Bad_DB,             /* bad registry database */
        Key_Not_Found,      /* registry key not found */
        Read_Error,         /* registry read error */
        Write_Error,        /* registry write error */
        Delete_Error,       /* registry delete error */
        Val_Not_Found,      /* registry value not found */
        No_Driver_Callback, /* driver does not call DriverCallback */
        More_Data,          /* more data to be returned */
      }

      MM_Msg :: enum u32 {
        // https://github.com/tpn/winsdk-7/blob/master/v7.1A/Include/MMSystem.h#L168
        Open       = 0x3C1,
        Close      = 0x3C2,
        Data       = 0x3C3,
        Long_Data  = 0x3C4,
        Error      = 0x3C5,
        Long_Error = 0x3C6,
      }

      MidiOpenFlag :: enum w32.DWORD {
        Null     = 0x00000000,
        Window   = 0x00010000,
        Thread   = 0x00020000,
        Function = 0x00030000,
        Event    = 0x00050000,
      }

    @(default_calling_convention="c")
    foreign lib {

      @(link_name="midiInOpen")
      midi_in_open :: proc(midi_handle : ^HMIDIIN, device_id : u32, callback : rawptr, data : rawptr, open_flag : MidiOpenFlag) -> MM_Result ---

      @(link_name="midiInClose")
      midi_in_close :: proc(midi_handle : HMIDIIN) -> MM_Result ---

      @(link_name="midiInStart")
      midi_in_start :: proc(midi_handle : HMIDIIN) -> MM_Result ---

      @(link_name="midiInStop")
      midi_in_stop :: proc(midi_handle : HMIDIIN) -> MM_Result ---
    }

  } else {

    #panic("Unsupported OS")

  }

// Config:
  // TODO (hitch) 2023-07-31 Config should be configurable... not hard coded ^_^

  GAME_FULLSCREEN :: false // put game in fullscreen mode
  GAME_ALLOW_MISSING :: false // UNIMPLEMENTED if notes should keep scrolling if you are not pressing the right keys
  GAME_SONG_TITLE :: "Happy Birthday" // hard coded song name
  MIDI_FILE_NAME :: "Happy Birthday.mid" // midi file to load notes from
  GAME_SPAWN_TIME :: 5 // spawn notes 5 seconds away

  // TODO (hitch) 2023-07-31 Remove these:
  MIDI_TICKS_PER_SECOND :: 300 // this should be read from the midi file, it's approximate set up for the `Happy Birthday.mid` file.
  GAME_SCROLL_SPEED :: 0.018 // how fast the notes scroll, this needs to be adjusted based on midi_ticks_per_second

  DeviceButtonInput :: union {
    Note,
    rl.KeyboardKey,
  }

  CONTROLS_SPEED_DOWN := []DeviceButtonInput{
    Note.C1,
    rl.KeyboardKey.A,
  }
  CONTROLS_SPEED_UP := []DeviceButtonInput{
    Note.Cs1,
    rl.KeyboardKey.Q,
  }
  CONTROLS_PAUSE_PLAY := []DeviceButtonInput{
    Note.D1,
    rl.KeyboardKey.SPACE,
  }
  CONTROLS_TOGGLE_LEFT := []DeviceButtonInput{
    Note.B5,
    rl.KeyboardKey.Z,
  }
  CONTROLS_TOGGLE_RIGHT := []DeviceButtonInput{
    Note.C6,
    rl.KeyboardKey.X,
  }

// Notes:

  Note :: enum u8 {
    None = 0,

    C1  = 36,
    Cs1 = 37,
    D1  = 38,
    Ds1 = 39,
    E1  = 40,
    F1  = 41,
    Fs1 = 42,
    G1  = 43,
    Gs1 = 44,
    A1  = 45,
    As1 = 46,
    B1  = 47,

    C2  = 48,
    Cs2 = 49,
    D2  = 50,
    Ds2 = 51,
    E2  = 52,
    F2  = 53,
    Fs2 = 54,
    G2  = 55,
    Gs2 = 56,
    A2  = 57,
    As2 = 58,
    B2  = 59,

    C3  = 60,
    Cs3 = 61,
    D3  = 62,
    Ds3 = 63,
    E3  = 64,
    F3  = 65,
    Fs3 = 66,
    G3  = 67,
    Gs3 = 68,
    A3  = 69,
    As3 = 70,
    B3  = 71,

    C4  = 72,
    Cs4 = 73,
    D4  = 74,
    Ds4 = 75,
    E4  = 76,
    F4  = 77,
    Fs4 = 78,
    G4  = 79,
    Gs4 = 80,
    A4  = 81,
    As4 = 82,
    B4  = 83,

    C5  = 84,
    Cs5 = 85,
    D5  = 86,
    Ds5 = 87,
    E5  = 88,
    F5  = 89,
    Fs5 = 90,
    G5  = 91,
    Gs5 = 92,
    A5  = 93,
    As5 = 94,
    B5  = 95,

    C6  = 96,
  }

  NATURAL_NOTES :: []Note{
    .C1,
    .D1,
    .E1,
    .F1,
    .G1,
    .A1,
    .B1,

    .C2,
    .D2,
    .E2,
    .F2,
    .G2,
    .A2,
    .B2,

    .C3,
    .D3,
    .E3,
    .F3,
    .G3,
    .A3,
    .B3,

    .C4,
    .D4,
    .E4,
    .F4,
    .G4,
    .A4,
    .B4,

    .C5,
    .D5,
    .E5,
    .F5,
    .G5,
    .A5,
    .B5,

    .C6,
  }

  SHARP_NOTES :: []Note{
    .Cs1,
    .Ds1,
    .None,
    .Fs1,
    .Gs1,
    .As1,
    .None,

    .Cs2,
    .Ds2,
    .None,
    .Fs2,
    .Gs2,
    .As2,
    .None,

    .Cs3,
    .Ds3,
    .None,
    .Fs3,
    .Gs3,
    .As3,
    .None,

    .Cs4,
    .Ds4,
    .None,
    .Fs4,
    .Gs4,
    .As4,
    .None,

    .Cs5,
    .Ds5,
    .None,
    .Fs5,
    .Gs5,
    .As5,
    .None,
  }

// Game State:

  running := true

  game_combo := f32(1)
  game_score := f32(0)
  game_pause := false
  game_time_scale := f32(1.0)
  game_hands : bit_set[enum{ LEFT, RIGHT }] = { .LEFT, .RIGHT }

  keyboard_state : bit_set[Note]
  last_keyboard_state : bit_set[Note]
  last_playback_state : bit_set[Note]
  playback_head : #sparse [Note]^NoteEntity
  playback_entities := make([]NoteEntity, 256)
  add_entity_index : u8

  NoteEntityFlag :: enum {
    Active,
    Hit_Start,
    Holding,
    Missed,
    Second_Track,
  }
  NoteEntity :: struct {
    note : Note,
    lead_time : i64,
    end_time : i64,
    flags : bit_set[NoteEntityFlag],
  }

// Midi Input Callback:

  midi_input_callback :: proc "c" (midi_in : HMIDIIN, msg : MM_Msg, data : rawptr, param_1, param_2 : w32.DWORD) {
    context = (^runtime.Context)(data)^
    #partial switch msg {
      case .Data:
        MidiStatus :: enum u8 {
          Note_Off              = 0b1000_0000,
          Note_On               = 0b1001_0000,
          Polyphonic_Aftertouch = 0b1010_0000,
          Control               = 0b1011_0000,
          Program_Change        = 0b1100_0000,
          Aftertouch            = 0b1101_0000,
          Pitch_Bend            = 0b1110_0000,
          System_Exclusive      = 0b1111_0000,
          Time_Code_Qtr_Frame   = 0b1111_0001,
          Song_Position_Pointer = 0b1111_0010,
          Song_Select           = 0b1111_0011,
                              _ = 0b1111_0100,
                              _ = 0b1111_0101,
          Tune_Request          = 0b1111_0110,
          End_Of_SysEx          = 0b1111_0111,
          Timing_Clock          = 0b1111_1000,
                              _ = 0b1111_1001,
          Start                 = 0b1111_1010,
          Continue              = 0b1111_1011,
          Stop                  = 0b1111_1100,
                              _ = 0b1111_1101,
          Active_Sensing        = 0b1111_1110,
          System_Reset          = 0b1111_1111,
        }

        status := MidiStatus(param_1 & 0xF0)
        if status == .System_Exclusive {
          status = MidiStatus(param_1 & 0xFF)
        }
        channel := u8(param_1        & 0x0F)
        data_1 := u8((param_1 >>  8) & 0xFF)
        data_2 := u8((param_1 >> 16) & 0xFF)
        timestamp := time.Duration(param_2) * time.Millisecond

        #partial switch status {
          case .Note_On:
            note := Note(data_1)
            velocity := data_2
            if velocity > 0 {
              if slice.contains(CONTROLS_SPEED_DOWN, note) {
                game_time_scale *= 0.95
              } else if slice.contains(CONTROLS_SPEED_UP, note) {
                game_time_scale *= 1.0526315789473684
              } else if slice.contains(CONTROLS_PAUSE_PLAY, note) {
                game_pause = !game_pause
              } else if slice.contains(CONTROLS_TOGGLE_LEFT, note) {
                game_hands ~= { .LEFT }
              } else if slice.contains(CONTROLS_TOGGLE_RIGHT, note) {
                game_hands ~= { .RIGHT }
              } else {
                keyboard_state |= { note }
              }
            } else {
              keyboard_state &= ~{ note }
            }
          case .Note_Off:
            note := Note(data_1)
            keyboard_state &= ~{ note }
        }
    }
  }

// Main:

  main :: proc() {
    // WinMM Midi:
      midi : HMIDIIN

      ctx := context
      if result := midi_in_open(&midi, 0, rawptr(midi_input_callback), &ctx, .Function); result != .Success {
        fmt.eprintf("Could not open midi device: %v\n", result)
        os.exit(1)
      }
      defer midi_in_close(midi)

      if result := midi_in_start(midi); result != .Success {
        fmt.eprintf("Could not start midi device: %v\n", result)
        os.exit(1)
      }
      defer midi_in_stop(midi)

    // Midi File:
      TRANSPOSE :: 0
      midi_data := #load(MIDI_FILE_NAME)
      if string(midi_data[:4]) != "MThd" {
        fmt.eprintln(MIDI_FILE_NAME + " :: midi header missing")
        os.exit(1)
      }
      midi_data = midi_data[4:]
      header_size := (u32(midi_data[0]) << 24) + (u32(midi_data[1]) << 16) + (u32(midi_data[2]) << 8) + u32(midi_data[3])
      midi_format := (u16(midi_data[4]) << 8) + u16(midi_data[5])
      midi_ntracks := (u16(midi_data[6]) << 8) + u16(midi_data[7])
      if !((midi_format == 0 || midi_ntracks == 1) || (midi_format == 1 && midi_ntracks == 2)) {
        fmt.eprintln(MIDI_FILE_NAME + " :: midi file must have a sigle, or left and right track")
        os.exit(1)
      }
      midi_tickdiv := (u16(midi_data[8]) << 8) + u16(midi_data[9])
      midi_ticks_per_second : f64
      // TODO (hitch) 2023-07-31 Read the timing or tempo from the midi file, and update tick/sec via
      //       midi messages.
      midi_ticks_per_second = MIDI_TICKS_PER_SECOND
      /*
      if midi_tickdiv & 0b10000000_00000000 > 0 {
        // Timecode
        fps := -i8(midi_data[8])
        fmt.println(MIDI_FILE_NAME + " FPS:", fps, "Subs:", midi_data[9])
        midi_ticks_per_second = f64(fps) * f64(midi_data[9])
      } else {
        // Metrical Timing
        ppqn := (midi_tickdiv & 0b01111111_11111111)
        fmt.println(MIDI_FILE_NAME + " PPQN:", ppqn)
        // IDK if this is right, it assumes a BPM of 120
        midi_ticks_per_second = 0.5 / f64(ppqn)
      }
      */

      midi_data = midi_data[4+header_size:]
      if string(midi_data[:4]) != "MTrk" {
        fmt.eprintln(MIDI_FILE_NAME + " :: midi track missing")
        os.exit(1)
      }
      midi_data = midi_data[8:]
      TimedNotes :: struct{ tick_time : i64, left_on, left_off, right_on, right_off : bit_set[Note] }
      loading_timed_notes : map[i64]TimedNotes
      tick_time : i64
      loading_right := false
      read_midi:
      for len(midi_data) > 0 {
        update_timed_notes :: proc(timed_notes : ^map[i64]TimedNotes, tick_time : i64, is_right : bool, keys_on, keys_off : bit_set[Note]) {
          if _, ok := timed_notes[tick_time]; ok {
            if is_right {
              (&timed_notes[tick_time]).right_on |= keys_on
              (&timed_notes[tick_time]).right_off |= keys_off
            } else {
              (&timed_notes[tick_time]).left_on |= keys_on
              (&timed_notes[tick_time]).left_off |= keys_off
            }
            return
          }
          if is_right {
            timed_notes[tick_time] = TimedNotes{ tick_time, {}, {}, keys_on, keys_off }
          } else {
            timed_notes[tick_time] = TimedNotes{ tick_time, keys_on, keys_off, {}, {} }
          }
        }
        read_byte :: proc(stream : ^[]u8) -> (result : u8) {
          result = stream[0]
          stream^ = stream[1:]
          return
        }
        read_num :: proc(stream : ^[]u8) -> (result : u32) {
          for {
            b := stream[0]
            result <<= 7
            result += u32(b & 0b0111_1111)
            stream^ = stream[1:]
            if (b & 0b1000_0000) == 0 {
              break
            }
          }
          return
        }
        delta_time := read_num(&midi_data)
        tick_time += i64(delta_time)
        event_type := read_byte(&midi_data)
        if event_type & 0b1000_0000 > 0 {
          if event_type == 0xFF {
            meta_type := read_byte(&midi_data)
            meta_length := read_num(&midi_data)
            meta_data := midi_data[:meta_length]
            midi_data = midi_data[meta_length:]
            switch meta_type {
              case 0x58:
                // Time Signature
                numerator := meta_data[0]
                denominator := meta_data[1]
                ticks_per_click := meta_data[2]
                _32nd_per_4th := meta_data[3]

              case 0x51:
                // Tempo
                tempo := u32(meta_data[0] << 16) + u32(meta_data[1] << 8) + u32(meta_data[2])

              case 0x03:
                // Track Name:
                track_name := string(meta_data)

              case 0x2F:
                // Track End
                tick_time = 0
                loading_right = true
                if len(midi_data) < 4 || string(midi_data[:4]) != "MTrk" {
                  break read_midi
                }
                midi_data = midi_data[8:]

              case:
                fmt.eprintf("Unhandeled Meta-Event Message 0x%2X\n", meta_type)
                // os.exit(1)
            }
          } else if event_type & 0xF0 == 0xF0 {
            switch event_type {
              case 0b1010_0000:
                // Polyphonic Key Preassure
                fallthrough
              case 0b1011_0000:
                // Control Change
                fallthrough
              case 0b1100_0000:
                // Program Change
                fallthrough
              case 0b1101_0000:
                // Channel Preassure
                fallthrough
              case 0b1110_0000:
                // Pitch Wheel Change
                fallthrough
              case 0b1111_0000:
                // System Exclusive
                fallthrough
              case:
                fmt.eprintf("Unhandeled System Message 0b%8b\n", event_type)
                os.exit(1)
            }
          } else {
            switch event_type & 0xF0 {
              case 0b1000_0000:
                // Note Off
                note := Note(TRANSPOSE + i8(read_byte(&midi_data)))
                velocity := read_byte(&midi_data)
                update_timed_notes(&loading_timed_notes, tick_time, loading_right, {}, { note })
                fmt.println(tick_time, "OFF", note)

              case 0b1001_0000:
                // Note On
                note := Note(TRANSPOSE + i8(read_byte(&midi_data)))
                velocity := read_byte(&midi_data)
                if velocity > 0 {
                  update_timed_notes(&loading_timed_notes, tick_time, loading_right, { note }, {})
                  fmt.println(tick_time, "ON", note)
                } else {
                  update_timed_notes(&loading_timed_notes, tick_time, loading_right, {}, { note })
                  fmt.println(tick_time, "OFF", note)
                }

              case 0b1100_0000:
                // Program Change
                program := read_byte(&midi_data)

              case 0b1011_0000:
                // Control/Mode Change
                control_function := read_byte(&midi_data)
                control_data := read_byte(&midi_data)

              case 0b1110_0000:
                // Pitch Bend
                lsb := read_byte(&midi_data)
                msb := read_byte(&midi_data)

              case:
                fmt.eprintf("Unhandeled MIDI Message 0b%4b_nnnn\n", (event_type >> 4))
                os.exit(1)
            }
          }
        } else {
          switch event_type {
            case:
              fmt.eprintf("Unhandeled Controller Message 0x%2X\n", event_type)
              os.exit(1)
          }
        }
      }
      timed_notes := make([]TimedNotes, len(loading_timed_notes))
      i := 0
      for _, tn in loading_timed_notes {
        timed_notes[i] = tn
        i += 1
      }
      delete(loading_timed_notes)
      slice.sort_by(timed_notes, proc(l, r : TimedNotes) -> bool {
        return l.tick_time < r.tick_time
      })

    // Raylib Setup:
      rl.InitWindow(800, 450, "Piano Thing")
      rl.SetTargetFPS(144)

      // TODO (hitch) 2023-07-31 Add full screen toggle:
      when GAME_FULLSCREEN {
        monitor := rl.GetCurrentMonitor()
        rl.SetWindowSize(rl.GetMonitorWidth(monitor), rl.GetMonitorHeight(monitor))
        rl.ToggleFullscreen()
      }

      defer rl.CloseWindow()

      camera := rl.Camera{
        position = { 0, 17, 10 },
        target = { 0, 0, -6 },
        up = { 0, 1, 0 },
        fovy = 60,
        projection = .PERSPECTIVE,
      }

      playback_shader := rl.LoadShaderFromMemory(
        `#version 330
        in vec3 vertexPosition;
        in vec2 vertexTexCoord;
        in vec3 vertexNormal;
        in vec4 vertexColor;

        uniform mat4 mvp;
        uniform mat4 matModel;

        void main() {
          vec3 vp = vertexPosition;
          vec3 world_pos = (matModel * vec4(vp, 1.0)).xyz;
          vp.y = -0.09 * world_pos.z;
          vp.y *= vp.y;
          // 7 * math.sin(max(0, f32(entity.lead_time)/f32(GAME_SPAWN_TIME*midi_ticks_per_second)) * (math.PI/2))
          gl_Position = mvp*vec4(vp, 1.0);
        }`,
        ////////////////////////////////////////////////////////////////////////
        `#version 330

        uniform vec4 colDiffuse;

        out vec4 finalColor;

        void main() {
          finalColor = colDiffuse;
        }`)
      verts : [4*50][3]f32
      indicies : [6*50]u16
      for i in 0..<50 {
        verts[4*i + 0] = { -0.5, 0, -0.5 + 0.02*f32(i+1) }
        verts[4*i + 1] = {  0.5, 0, -0.5 + 0.02*f32(i+1) }
        verts[4*i + 2] = { -0.5, 0, -0.5 + 0.02*f32(i) }
        verts[4*i + 3] = {  0.5, 0, -0.5 + 0.02*f32(i) }
        indicies[6*i + 0] = u16(0 + 4*i)
        indicies[6*i + 1] = u16(1 + 4*i)
        indicies[6*i + 2] = u16(2 + 4*i)
        indicies[6*i + 3] = u16(2 + 4*i)
        indicies[6*i + 4] = u16(1 + 4*i)
        indicies[6*i + 5] = u16(3 + 4*i)
      }
      mesh := rl.Mesh{
        triangleCount = 50 * 2,
        vertexCount = 50 * 4,
        vertices = &verts[0][0],
        indices = &indicies[0],
      }
      rl.UploadMesh(&mesh, false)
      playback_note := rl.LoadModelFromMesh(mesh)
      playback_note.materials[0].shader = playback_shader

      key_shader := rl.LoadShaderFromMemory(
        `#version 330
        in vec3 vertexPosition;
        in vec2 vertexTexCoord;
        in vec3 vertexNormal;
        in vec4 vertexColor;

        uniform mat4 mvp;
        uniform mat4 matNormal;

        out vec3 fragNormal;

        void main() {
          gl_Position = mvp*vec4(vertexPosition, 1.0);
          fragNormal = normalize(vec3(matNormal*vec4(vertexNormal, 1.0)));
        }`,
        ////////////////////////////////////////////////////////////////////////
        `#version 330

        in vec3 fragNormal;

        uniform vec4 colDiffuse;

        out vec4 finalColor;

        void main() {
          float atten = (0.5 * dot(fragNormal, vec3(0, 1, 0))) + 0.5;
          finalColor = vec4(atten * colDiffuse.xyz, 1);
        }`)

      white_key := rl.LoadModelFromMesh(rl.GenMeshCube(0.9, 0.5, 5.5))
      white_key.materials[0].shader = key_shader
      black_key := rl.LoadModelFromMesh(rl.GenMeshCube(0.55, 0.5, 4))
      black_key.materials[0].shader = key_shader

    // Game Loop:
      tick_time = 0
      playback_index := 0
      tick_acc := 0.0
      for !rl.WindowShouldClose() {
        mem.free_all(context.temp_allocator)

        // Update:
          key := rl.GetKeyPressed()
          if slice.contains(CONTROLS_SPEED_DOWN, key) {
            game_time_scale *= 0.95
          } else if slice.contains(CONTROLS_SPEED_UP, key) {
            game_time_scale *= 1.0526315789473684
          } else if slice.contains(CONTROLS_PAUSE_PLAY, key) {
            game_pause = !game_pause
          } else if slice.contains(CONTROLS_TOGGLE_LEFT, key) {
            game_hands ~= { .LEFT }
          } else if slice.contains(CONTROLS_TOGGLE_RIGHT, key) {
            game_hands ~= { .RIGHT }
          }

          pressed_keys := keyboard_state & ~last_keyboard_state
          bad_presses := pressed_keys
          last_keyboard_state = keyboard_state

          if !game_pause {
            tick_acc += f64(game_time_scale) * midi_ticks_per_second * f64(rl.GetFrameTime())
          }
          delta_tick := u64(tick_acc)
          tick_acc -= f64(delta_tick)
          tick_time += i64(delta_tick)
          // Entities:
            for entity in &playback_entities {
              if .Active in entity.flags {
                entity.lead_time -= i64(delta_tick)
                entity.end_time -= i64(delta_tick)
                if entity.end_time < 0 {
                  entity.flags &= ~{ .Active }
                } else {
                  if .Second_Track in entity.flags {
                    if .LEFT not_in game_hands {
                      continue
                    }
                  } else {
                    if .RIGHT not_in game_hands {
                      continue
                    }
                  }

                  if entity.lead_time < 200 && .Missed not_in entity.flags {
                    if .Holding in entity.flags {
                      if entity.note not_in keyboard_state {
                        // stopped holding
                        entity.flags &= ~{ .Holding }
                        entity.flags |= { .Missed }
                      } else {
                        // still holding
                        game_score += game_combo * 4 * f32(delta_tick) / (game_time_scale * f32(midi_ticks_per_second))
                      }
                    }
                    if .Hit_Start not_in entity.flags {
                      if entity.note in pressed_keys {
                        bad_presses &= ~{ entity.note }
                        if entity.lead_time < 100 && entity.lead_time > -100 {
                          // note hit
                          d := f32(100 - math.abs(entity.lead_time))
                          game_score += game_combo * 0.06 * d * d
                          game_combo += 0.1
                          entity.flags |= { .Hit_Start }
                          entity.flags |= { .Holding }
                        } else {
                          // TODO (hitch) 2023-07-28 If there is another short note before this one,
                          //       you can miss a note behind the one you are playing... We will have
                          //       to detect notes behind notes.
                          // to early
                          // game_combo = 1
                          // entity.flags |= { .Missed }
                        }
                      } else if entity.lead_time < -100 {
                        // to late
                        game_combo = 1
                        entity.flags |= { .Missed }
                      }
                    }
                  }
                }
              }
            }
            if bad_presses != {} {
              // wrong note
              game_combo = 1
            }

          // Playback:
            if playback_index >= len(timed_notes) {
              playback_index = 0
              tick_time = -i64(GAME_SPAWN_TIME * midi_ticks_per_second)
            }
            if tick_time >= timed_notes[playback_index].tick_time {
              for note in Note {
                if note in timed_notes[playback_index].left_off || note in timed_notes[playback_index].right_off {
                  playback_head[note].end_time = i64((GAME_SPAWN_TIME-0.1) * midi_ticks_per_second)
                  playback_head[note] = nil
                }
                if note in timed_notes[playback_index].left_on || note in timed_notes[playback_index].right_on {
                  playback_entities[add_entity_index] = {
                    note = note,
                    lead_time = i64(GAME_SPAWN_TIME * midi_ticks_per_second),
                    end_time = max(i64),
                    flags = (note in timed_notes[playback_index].right_on) ? { .Active, .Second_Track } : { .Active },
                  }
                  playback_head[note] = &playback_entities[add_entity_index]
                  add_entity_index += 1
                }
              }
              playback_index += 1
            }

        // Draw:
          rl.BeginDrawing()
            BACKGROUND_COLOR :: rl.Color{ 0x55, 0x33, 0x55, 0xFF }
            WRONG_COLOR :: rl.Color{ 0xFF, 0x99, 0x99, 0xFF }
            MISSED_COLOR :: rl.Color{ 0x88, 0x88, 0x88, 0xFF }

            LEFT_CORRECT_COLOR :: rl.Color{ 0xAA, 0xDD, 0xFF, 0xFF }
            RIGHT_CORRECT_COLOR :: rl.Color{ 0xDD, 0xFF, 0xAA, 0xFF }

            rl.ClearBackground(BACKGROUND_COLOR)
            rl.DrawText(fmt.ctprintf("Song: %v", GAME_SONG_TITLE), 5, 5, 20, rl.RAYWHITE)
            if .RIGHT in game_hands {
              if .LEFT in game_hands {
                rl.DrawText("Hands: Right + Left", 5, 30, 20, rl.LIGHTGRAY)
              } else {
                rl.DrawText("Hands: Right", 5, 30, 20, rl.LIGHTGRAY)
              }
            } else if .LEFT in game_hands {
              rl.DrawText("Hands: Left", 5, 30, 20, rl.LIGHTGRAY)
            } else {
              rl.DrawText("Hands: none", 5, 30, 20, rl.LIGHTGRAY)
            }
            rl.DrawText(fmt.ctprintf("Speed: x%0.2v %v", game_time_scale, game_pause ? "~PAUSED~" : ""), 5, 55, 20, rl.SKYBLUE)

            w := rl.GetScreenWidth()
            ts := fmt.ctprintf("Score: %v", int(game_score))
            rl.DrawText(ts, w-5-rl.MeasureText(ts, 30), 5, 30, rl.WHITE)
            ts = fmt.ctprintf("Combo: x%0.1v", game_combo)
            rl.DrawText(ts, w-5-rl.MeasureText(ts, 30), 40, 30, rl.WHITE)

            rl.BeginMode3D(camera)

              current_keys : bit_set[Note]
              key_entities : #sparse [Note]^NoteEntity
              for entity in &playback_entities {
                if .Active in entity.flags {
                  if entity.lead_time < 0 {
                    key_entities[entity.note] = &entity
                    current_keys |= { entity.note }
                  }
                }
              }

              x := note_x_pos(NATURAL_NOTES[0])
              for note in NATURAL_NOTES {
                FILL_COLOR :: rl.Color{ 0xDD, 0xDD, 0xDD, 0xFF }

                if note in keyboard_state {
                  if note in current_keys {
                    if .Second_Track in key_entities[note].flags {
                      rl.DrawModel(white_key, { x, -0.3, 2.75 }, 1, RIGHT_CORRECT_COLOR)
                    } else {
                      rl.DrawModel(white_key, { x, -0.3, 2.75 }, 1, LEFT_CORRECT_COLOR)
                    }
                  } else {
                    rl.DrawModel(white_key, { x, -0.3, 2.75 }, 1, WRONG_COLOR)
                  }
                } else {
                  rl.DrawModel(white_key, { x, 0, 2.75 }, 1, FILL_COLOR)
                }
                x += 1
              }

              x = note_x_pos(SHARP_NOTES[0])
              for note in SHARP_NOTES {
                FILL_COLOR :: rl.Color{ 0x33, 0x33, 0x33, 0xFF }

                if note != .None {
                  if note in keyboard_state {
                    if note in current_keys {
                      if .Second_Track in key_entities[note].flags {
                        rl.DrawModel(black_key, { x, 0.05, 2 }, 1, RIGHT_CORRECT_COLOR)
                      } else {
                        rl.DrawModel(black_key, { x, 0.05, 2 }, 1, LEFT_CORRECT_COLOR)
                      }
                    } else {
                      rl.DrawModel(black_key, { x, 0.05, 2 }, 1, WRONG_COLOR)
                    }
                  } else {
                      rl.DrawModel(black_key, { x, 0.3, 2 }, 1, FILL_COLOR)
                  }
                }
                x += 1
              }

              for entity in playback_entities {
                if .Active in entity.flags {
                  if .Second_Track in entity.flags {
                    if .LEFT not_in game_hands {
                      continue
                    }
                  } else {
                    if .RIGHT not_in game_hands {
                      continue
                    }
                  }
                  x := note_x_pos(entity.note)
                  min_y := GAME_SCROLL_SPEED * max(0, f32(entity.lead_time)) / game_time_scale
                  max_y := GAME_SCROLL_SPEED * min(GAME_SPAWN_TIME*f32(midi_ticks_per_second), f32(entity.end_time)) / game_time_scale
                  color := MISSED_COLOR
                  if .Missed not_in entity.flags {
                    if .Second_Track in entity.flags {
                      color = RIGHT_CORRECT_COLOR
                    } else {
                      color = LEFT_CORRECT_COLOR
                    }
                  }
                  rl.DrawModelEx(playback_note, { x, 0, (-max_y - min_y)/2 }, {0, 1, 0}, 0, { note_is_sharp(entity.note) ? 0.5 : 0.9, 1, (max_y - min_y) }, color)
                }
              }
            rl.EndMode3D()
          rl.EndDrawing()
    }
  }

// Utility:

  note_x_pos :: proc(note : Note) -> (x : f32) {
    x = -17.5
    for n in NATURAL_NOTES {
      if n == note {
        return
      }
      x += 1
    }

    x = -17
    for n in SHARP_NOTES {
      if n == note {
        return
      }
      x += 1
    }

    return x
  }

  note_is_sharp :: proc(note : Note) -> bool {
    for n in SHARP_NOTES {
      if n == note {
        return true
      }
    }
    return false
  }
