(* -*- mode: sml; mode: read-only -*- *)
(* This file was generated by util/record.lua. Do not edit by hand! *)
structure CommandLineSettings = struct
  structure set = struct
     val subcommand = fn subcommand => fn { subcommand = _, output, outputMode, dump, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val output = fn output => fn { subcommand, output = _, outputMode, dump, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val outputMode = fn outputMode => fn { subcommand, output, outputMode = _, dump, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val dump = fn dump => fn { subcommand, output, outputMode, dump = _, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val optimizationLevel = fn optimizationLevel => fn { subcommand, output, outputMode, dump, optimizationLevel = _, backend, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val backend = fn backend => fn { subcommand, output, outputMode, dump, optimizationLevel, backend = _, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val libDir = fn libDir => fn { subcommand, output, outputMode, dump, optimizationLevel, backend, libDir = _ } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
  end
  structure update = struct
     val subcommand = fn f => fn { subcommand, output, outputMode, dump, optimizationLevel, backend, libDir } => { subcommand = f subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val output = fn f => fn { subcommand, output, outputMode, dump, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = f output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val outputMode = fn f => fn { subcommand, output, outputMode, dump, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = output, outputMode = f outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val dump = fn f => fn { subcommand, output, outputMode, dump, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = f dump, optimizationLevel = optimizationLevel, backend = backend, libDir = libDir }
     val optimizationLevel = fn f => fn { subcommand, output, outputMode, dump, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = f optimizationLevel, backend = backend, libDir = libDir }
     val backend = fn f => fn { subcommand, output, outputMode, dump, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = f backend, libDir = libDir }
     val libDir = fn f => fn { subcommand, output, outputMode, dump, optimizationLevel, backend, libDir } => { subcommand = subcommand, output = output, outputMode = outputMode, dump = dump, optimizationLevel = optimizationLevel, backend = backend, libDir = f libDir }
  end
end;
