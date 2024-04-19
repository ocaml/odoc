type switch = [ `On | `Off ]

(**

   References with type as parent works:
   - {!type-switch.On}
   - {!type-switch.`Off}
   - {!type-switch.constructor-On}
   - {!type-switch.constructor-`Off}
   - {!switch.On}
   - {!switch.`Off}
   - {!switch.constructor-On}
   - {!switch.constructor-`Off}

   References in the environment don't work:
   - {!On}
   - {!`On}
   - {!constructor-On}
   - {!constructor-`On} *)
