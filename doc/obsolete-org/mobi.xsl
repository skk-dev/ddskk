<?xml version='1.0'?>
<!--
Copyright (c) 2010 Kan-Ru Chen

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-->
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:ncx="http://www.daisy.org/z3986/2005/ncx/"
    version="1.0">

<xsl:import href="/usr/share/xml/docbook/stylesheet/docbook-xsl/epub/docbook.xsl"/>

<!-- Kindle dosen't handle dl table very well -->
<xsl:param name="toc.list.type">ul</xsl:param>

<!-- Flatten navPoints and kindlegen fix
     Copied from docbook.xsl, remove the ncx: prefix since kindlegen cannot handle it.
     Generate flat navPoints because kindle doesn't understand nested navPoints.
 -->
  <xsl:template name="ncx">
    <xsl:call-template name="write.chunk">
      <xsl:with-param name="filename">
        <xsl:if test="$manifest.in.base.dir != 0">
          <xsl:value-of select="$base.dir" />
        </xsl:if>
        <xsl:value-of select="$epub.ncx.filename" />
      </xsl:with-param>
      <xsl:with-param name="method" select="'xml'" />
      <xsl:with-param name="encoding" select="'utf-8'" />
      <xsl:with-param name="indent" select="'yes'" />
      <xsl:with-param name="quiet" select="$chunk.quietly" />
      <xsl:with-param name="doctype-public" select="''"/> <!-- intentionally blank -->
      <xsl:with-param name="doctype-system" select="''"/> <!-- intentionally blank -->
      <xsl:with-param name="content">
        <xsl:element name="ncx">
          <xsl:attribute name="version">2005-1</xsl:attribute>

            <!-- Via Martin Goerner: On covers: the IDPF2.0 standard unfortunately does not have a provision for
            covers. We had to add one and we did so in conjunction with the IDPF and
            various publishers. The tag chosen to define the covers is:
            <meta name="cover" content="-reference to a manifest item-">
            Then, we also added a bit of logic to get rid cleanly of the HTML cover
            people usually add because the logical cover is not specced by the IDPF. So,
            if the HTML cover item is marked linear="no" AND there is a guide item of
            type="cover" pointing to it AND there is a logical cover specified in a
            <meta name="cover"> tag, THEN, the HTML cover is discarded. -->
          <xsl:element name="head">
            <xsl:if test="/*/*[cover or contains(name(.), 'info')]//mediaobject[@role='cover' or ancestor::cover]"> 
              <xsl:element name="meta">
                <xsl:attribute name="name">cover</xsl:attribute>
                <xsl:attribute name="content">
                  <xsl:value-of select="$epub.cover.id"/>
                </xsl:attribute>
              </xsl:element>
            </xsl:if>
            <xsl:if test="/*/*[contains(name(.), 'info')]/isbn"> 
              <xsl:element name="meta">
                <xsl:attribute name="name">dtb:uid</xsl:attribute>
                <xsl:attribute name="content">
                  <xsl:text>isbn:</xsl:text>
                  <xsl:value-of select="/*/*[contains(name(.), 'info')]/isbn"/> 
                </xsl:attribute>
              </xsl:element>
            </xsl:if>
            <!-- TODO: be nice to have a name="cover" here for .mobi-->

            <!-- TODO What are these hardcoded values? -->
            <xsl:element name="meta">
              <xsl:attribute name="name">dtb:depth</xsl:attribute>
              <xsl:attribute name="content">-1</xsl:attribute>
            </xsl:element>
            <xsl:element name="meta">
              <xsl:attribute name="name">dtb:totalPageCount</xsl:attribute>
              <xsl:attribute name="content">0</xsl:attribute>
            </xsl:element>
            <xsl:element name="meta">
              <xsl:attribute name="name">dtb:maxPageNumber</xsl:attribute>
              <xsl:attribute name="content">0</xsl:attribute>
            </xsl:element>
          </xsl:element>
          <xsl:choose>
            <xsl:when test="$rootid != ''">
              <xsl:variable name="title">
                <xsl:if test="$epub.autolabel != 0">
                  <xsl:variable name="label.markup">
                    <xsl:apply-templates select="key('id',$rootid)" mode="label.markup" />
                  </xsl:variable>
                  <xsl:if test="normalize-space($label.markup)">
                    <xsl:value-of select="concat($label.markup,$autotoc.label.separator)" />
                  </xsl:if>
                </xsl:if>
                <xsl:apply-templates select="key('id',$rootid)" mode="title.markup" />
              </xsl:variable>
              <xsl:variable name="href">
                <xsl:call-template name="href.target.with.base.dir">
                  <xsl:with-param name="object" select="key('id',$rootid)" />
                </xsl:call-template>
              </xsl:variable>
              <xsl:element name="docTitle">
                <xsl:element name="text"><xsl:value-of select="normalize-space($title)" />  </xsl:element>
              </xsl:element>
              <xsl:element name="navMap">
                <xsl:apply-templates select="key('id',$rootid)/*" mode="ncx" />
              </xsl:element>
            </xsl:when>
            <xsl:otherwise>
              <xsl:variable name="title">
                <xsl:if test="$epub.autolabel != 0">
                  <xsl:variable name="label.markup">
                    <xsl:apply-templates select="/*" mode="label.markup" />
                  </xsl:variable>
                  <xsl:if test="normalize-space($label.markup)">
                    <xsl:value-of select="concat($label.markup,$autotoc.label.separator)" />
                  </xsl:if>
                </xsl:if>
                <xsl:apply-templates select="/*" mode="title.markup" />
              </xsl:variable>
              <xsl:variable name="href">
                <xsl:call-template name="href.target.with.base.dir">
                  <xsl:with-param name="object" select="/" />
                </xsl:call-template>
              </xsl:variable>
              <xsl:element name="docTitle">
                <xsl:element name="text">
                  <xsl:value-of select="normalize-space($title)" />
                </xsl:element>
              </xsl:element>
              <xsl:element name="navMap">
                <xsl:choose>
                  <xsl:when test="$root.is.a.chunk != '0'">
                    <xsl:apply-templates select="/*" mode="ncx" />
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:apply-templates select="/*/*" mode="ncx" />
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:element>
            </xsl:otherwise>

          </xsl:choose>
        </xsl:element>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="book|
                       article|
                       part|
                       reference|
                       preface|
                       chapter|
                       bibliography|
                       appendix|
                       glossary|
                       section|
                       sect1|
                       sect2|
                       sect3|
                       sect4|
                       sect5|
                       refentry|
                       colophon|
                       bibliodiv[title]|
                       setindex|
                       index"
                mode="ncx">
    <xsl:variable name="depth" select="count(ancestor::*)"/>
    <xsl:variable name="title">
      <xsl:if test="$epub.autolabel != 0">
        <xsl:variable name="label.markup">
          <xsl:apply-templates select="." mode="label.markup" />
        </xsl:variable>
        <xsl:if test="normalize-space($label.markup)">
          <xsl:value-of
            select="concat($label.markup,$autotoc.label.separator)" />
        </xsl:if>
      </xsl:if>
      <xsl:apply-templates select="." mode="title.markup" />
    </xsl:variable>

    <xsl:variable name="href">
      <xsl:call-template name="href.target.with.base.dir">
        <xsl:with-param name="context" select="/" />
        <!-- Generate links relative to the location of root file/toc.xml file -->
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="id">
      <xsl:value-of select="generate-id(.)"/>
    </xsl:variable>
    <xsl:variable name="order">
      <xsl:value-of select="$depth +
                                  count(preceding::part|
                                  preceding::reference|
                                  preceding::book[parent::set]|
                                  preceding::preface|
                                  preceding::chapter|
                                  preceding::bibliography|
                                  preceding::appendix|
                                  preceding::article|
                                  preceding::glossary|
                                  preceding::section[not(parent::partintro)]|
                                  preceding::sect1[not(parent::partintro)]|
                                  preceding::sect2|
                                  preceding::sect3|
                                  preceding::sect4|
                                  preceding::sect5|
                                  preceding::refentry|
                                  preceding::colophon|
                                  preceding::bibliodiv[title]|
                                  preceding::index)"/>
    </xsl:variable>

    <xsl:element name="navPoint">
      <xsl:attribute name="id">
        <xsl:value-of select="$id"/>
      </xsl:attribute>

      <xsl:attribute name="playOrder">
        <xsl:choose>
          <xsl:when test="/*[self::set]">
            <xsl:value-of select="$order"/>
          </xsl:when>
          <xsl:when test="$root.is.a.chunk != '0'">
            <xsl:value-of select="$order + 1"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$order - 0"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:element name="navLabel">
        <xsl:element name="text"><xsl:value-of select="normalize-space($title)"/> </xsl:element>
      </xsl:element>
      <xsl:element name="content">
        <xsl:attribute name="src">
          <xsl:value-of select="$href"/>
        </xsl:attribute>
      </xsl:element>
    </xsl:element>
    <xsl:apply-templates select="book[parent::set]|part|reference|preface|chapter|bibliography|appendix|article|glossary|section|sect1|sect2|sect3|sect4|sect5|refentry|colophon|bibliodiv[title]|setindex|index" mode="ncx"/>
  </xsl:template>

</xsl:stylesheet>
