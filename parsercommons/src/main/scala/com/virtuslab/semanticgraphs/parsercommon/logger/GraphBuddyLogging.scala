package com.virtuslab.semanticgraphs.parsercommon.logger

import ch.qos.logback.classic.Logger

trait GraphBuddyLogging { self =>
  given logger: Logger = LoggerFactory.getLogger(self.getClass)
}
