theories/Sockets.vo theories/Sockets.glob theories/Sockets.v.beautified theories/Sockets.required_vo: theories/Sockets.v 
theories/Sockets.vio: theories/Sockets.v 
theories/Sockets.vos theories/Sockets.vok theories/Sockets.required_vos: theories/Sockets.v 
theories/OCamlTypes.vo theories/OCamlTypes.glob theories/OCamlTypes.v.beautified theories/OCamlTypes.required_vo: theories/OCamlTypes.v theories/Sockets.vo
theories/OCamlTypes.vio: theories/OCamlTypes.v theories/Sockets.vio
theories/OCamlTypes.vos theories/OCamlTypes.vok theories/OCamlTypes.required_vos: theories/OCamlTypes.v theories/Sockets.vos
theories/StringTheory.vo theories/StringTheory.glob theories/StringTheory.v.beautified theories/StringTheory.required_vo: theories/StringTheory.v 
theories/StringTheory.vio: theories/StringTheory.v 
theories/StringTheory.vos theories/StringTheory.vok theories/StringTheory.required_vos: theories/StringTheory.v 
theories/Monads.vo theories/Monads.glob theories/Monads.v.beautified theories/Monads.required_vo: theories/Monads.v 
theories/Monads.vio: theories/Monads.v 
theories/Monads.vos theories/Monads.vok theories/Monads.required_vos: theories/Monads.v 
theories/Messages.vo theories/Messages.glob theories/Messages.v.beautified theories/Messages.required_vo: theories/Messages.v theories/OCamlTypes.vo theories/StringTheory.vo theories/Monads.vo
theories/Messages.vio: theories/Messages.v theories/OCamlTypes.vio theories/StringTheory.vio theories/Monads.vio
theories/Messages.vos theories/Messages.vok theories/Messages.required_vos: theories/Messages.v theories/OCamlTypes.vos theories/StringTheory.vos theories/Monads.vos
theories/Client.vo theories/Client.glob theories/Client.v.beautified theories/Client.required_vo: theories/Client.v theories/Messages.vo theories/Monads.vo
theories/Client.vio: theories/Client.v theories/Messages.vio theories/Monads.vio
theories/Client.vos theories/Client.vok theories/Client.required_vos: theories/Client.v theories/Messages.vos theories/Monads.vos
theories/PCExtract.vo theories/PCExtract.glob theories/PCExtract.v.beautified theories/PCExtract.required_vo: theories/PCExtract.v theories/Extraction.vo theories/Client.vo theories/OCamlTypes.vo theories/Monads.vo theories/Sockets.vo
theories/PCExtract.vio: theories/PCExtract.v theories/Extraction.vio theories/Client.vio theories/OCamlTypes.vio theories/Monads.vio theories/Sockets.vio
theories/PCExtract.vos theories/PCExtract.vok theories/PCExtract.required_vos: theories/PCExtract.v theories/Extraction.vos theories/Client.vos theories/OCamlTypes.vos theories/Monads.vos theories/Sockets.vos
