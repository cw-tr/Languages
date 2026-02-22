import os
import urllib.parse

# List of Categories and Languages
structure = {
    "0. Donanım Tanımlama ve İşlemci İçi Diller (Mutlak En Alt Seviye)": [
        "VHDL", "Verilog", "Mikrokod (Microcode)", "SystemVerilog", "Chisel"
    ],
    "1. Düşük Seviyeli Diller (Makineye En Yakın)": [
        "Makine Dili (Machine Code)", "Assembly Dili (ASM)", "WebAssembly (Wasm)", "LLVM IR", "PTX"
    ],
    "2. Tarihi, Kritik ve Kısıtlı Sistem Dilleri": [
        "Forth", "Fortran", "COBOL", "B Dili", "BCPL", "ALGOL", "Simula", "PL_I", "APL", "RPG", "SNOBOL", "Pascal", "Prolog", "Smalltalk", "Lisp"
    ],
    "3. Orta-Düşük Seviyeli Diller (Sistem ve Performans Dilleri)": [
        "C", "C++", "Rust", "Zig", "Ada", "CUDA", "OpenCL", "Nim", "D Dili (Dlang)", "Objective-C", "Delphi", "Vala"
    ],
    "4. Yüksek Seviyeli Genel Amaçlı Diller": [
        "Java", "C# (C-Sharp)", "Go (Golang)", "Swift", "Kotlin", "Scala", "Dart", "Visual Basic", "Groovy", "Haxe"
    ],
    "5. Fonksiyonel ve Eşzamanlı (Concurrent) Diller": [
        "Haskell", "Erlang", "Elixir", "Clojure", "F# (F-Sharp)", "OCaml", "Scheme", "Racket", "Standard ML"
    ],
    "6. Veri Bilimi ve Bilimsel Diller": [
        "R", "Julia", "MATLAB", "SAS", "SPSS", "Mathematica", "Octave", "Stata"
    ],
    "7. Veritabanı ve Kurumsal Spesifik (Domain-Specific) Diller": [
        "SQL", "ABAP", "PL_SQL", "T-SQL", "Apex", "ColdFusion", "GraphQL", "Cypher", "Regex"
    ],
    "8. Çok Yüksek Seviyeli Betik (Scripting) ve Otomasyon Dilleri": [
        "Python", "PHP", "Ruby", "Lua", "Bash", "PowerShell", "Perl", "Tcl", "VBScript", "Awk", "AppleScript"
    ],
    "9. Web ve Evrensel Arayüz Dilleri (En Yüksek Soyutlama)": [
        "JavaScript (JS)", "TypeScript (TS)", "CoffeeScript", "ActionScript", "GLSL"
    ],
    "10. Eğitsel ve Blok Tabanlı Diller": [
        "Scratch", "Logo", "Alice", "Blockly"
    ],
    "11. Ezoterik (Esoteric) ve Deneysel Diller": [
        "Brainfuck", "Malbolge", "Whitespace", "Chef", "Shakespeare", "INTERCAL", "Piet"
    ],
    "12. Biçimlendirme, Veri ve Tasarım Dilleri (Markup & Styling)": [
        "HTML", "CSS", "XML", "Markdown", "JSON", "YAML", "TOML", "Protocol Buffers (Protobuf)"
    ],
    "13. Modern Web Ekosistemleri ve Çerçeveleri (Frameworks & Runtimes)": [
        "ECMAScript", "Node.js", "React", "Next.js", "Vue.js", "Svelte", "Deno", "Bun"
    ],
    "14. Grafik Motorları ve Multimedya API'leri (Graphics & Multimedia)": [
        "OpenGL", "WebGL", "Flash (Platform)", "Silverlight"
    ],
    "15. Altyapı, Bulut Sistemleri ve DevOps Dilleri (IaC & CI-CD)": [
        "HCL (Terraform)", "Dockerfile", "Ansible"
    ],
    "16. Oyun Motoru ve Görsel Betik Dilleri (Game Scripting)": [
        "GDScript", "GML (GameMaker Language)", "Unreal Blueprints"
    ],
    "17. Blokzincir ve Akıllı Kontrat Dilleri (Web3 & Blockchain)": [
        "Solidity", "Vyper", "Move", "Cairo"
    ],
    "18. Belge Dizgi ve Sayfa Tanımlama Dilleri (Typesetting)": [
        "LaTeX (TeX)", "PostScript"
    ],
    "19. İnşa ve Derleme Otomasyonu Dilleri (Build Systems)": [
        "Make (Makefile)", "CMake", "Bazel"
    ],
    "20. Matematiksel İspat ve Formal Doğrulama Dilleri (Theorem Provers)": [
        "Coq", "TLA+"
    ],
    "21. Algoritmik Müzik ve Ses Programlama Dilleri": [
        "SuperCollider", "Sonic Pi"
    ],
    "22. Büyük Veri ve Analitik Sorgu Formatları (Big Data)": [
        "Parquet", "DAX"
    ],
    "23. Arama, Metrik ve Zaman-Serisi Sorgu Dilleri": [
        "Elasticsearch Query DSL", "PromQL"
    ],
    "24. Web Şablonlama (Templating) Dilleri": [
        "Jinja2", "Handlebars (Mustache)"
    ],
    "25. Masaüstü Otomasyon ve Fare/Klavye Betikleri": [
        "AutoHotkey (AHK)"
    ],
    "26. Araç Kontrol ve Sistem Görev Betikleri": [
        "TCL (Tool Command Language)"
    ],
    "27. Oyun Modlama ve Şablon Betikleri (Modding)": [
        "Papyrus", "UnrealScript"
    ]
}

# Add level 1.5 specifically for intermediate representations
structure["1.5. Ara Kod ve Derlenmiş Yığın Dilleri (Intermediate Representations)"] = [
    "Hexadecimal (Hex)", "Bytecode", "IL (Intermediate Language)"
]

base_dir = r"c:\CyberWorld\Dil Belgeleri"
md_content = """<p align="center">
  <a href="https://github.com/mukanerkin"><img src="https://img.shields.io/badge/Kurucu-Mukan_Erkin_T%C3%96R%C3%9CK-blue?style=flat-square&logo=github"></a>
  <a href="https://github.com/mukanerkin"><img src="https://img.shields.io/badge/Github-@mukanerkin-black?style=flat-square&logo=github"></a>
  <a href="mailto:mukanerkintoruk@gmail.com"><img src="https://img.shields.io/badge/Email-mukanerkintoruk@gmail.com-red?style=flat-square&logo=gmail"></a>
</p>

# 📚 Programlama Dilleri Ansiklopedisi: Düşük Seviyeden Yüksek Seviyeye

Bu depo (repository), bilgisayar bilimlerinin en derin noktası olan **Makine Dili'nden (0 ve 1'ler)** başlayarak, en yüksek soyutlama katmanı olan **Yapay Zeka (Prolog/LISP)**, **Akıllı Kontratlar (Solidity/Cairo)** ve **Veri Madenciliği (Elasticsearch/Parquet)** evrenlerine kadar uzanan devasa bir **Türkçe Programlama Dilleri ve Teknolojileri Ansiklopedisidir.**

Tarihin tozlu raflarındaki ölü dillerden (UnrealScript), roket fırlatan felsefi ispat dillerine (Coq, TLA+), müzik algoritmalarından (Sonic Pi) dev bilgisayar ağlarını yöneten betiklere (TCL) kadar evrendeki tüm programlama konseptleri seviyelere ayrılarak incelenmiştir. Her bir dilin kendi klasöründe *Nedir*, *Kimler Kullanır* ve *Mimari Mantığı/Kod Örneği* detaylıca belgelenmiştir.

> **💡 Proje Hakkında Yapay Zeka Notu:** Bu eşsiz ve muazzam eser; Mukan Erkin TÖRÜK'ün yöneticiliği ve vizyonu doğrultusunda, Google Deepmind ekibinin geliştirdiği **Gemini 3.1 Pro** modelini "Ajanik Planlama Modunda (Agentic Mode)" kullanan **Antigravity** yapay zeka kodlama asistanı tarafından ilmek ilmek araştırılıp, tasarlanıp, Türkçe olarak yazılmıştır.

---

"""

for category, langs in structure.items():
    md_content += f"## {category}\n"
    # Create category directory
    cat_path = os.path.join(base_dir, category)
    if not os.path.exists(cat_path):
        os.makedirs(cat_path)

    for lang in langs:
        lang_file = f"{lang}.md"
        lang_path = os.path.join(cat_path, lang_file)
        
        # Create empty .md file if not exists
        if not os.path.exists(lang_path):
            with open(lang_path, 'w', encoding='utf-8') as f:
                f.write(f"# {lang}\n\n## Özet\n\n## Nedir ve Ne İşe Yarar?\n\n## Dilin Mantığı ve Kod Yapısı\n\n## Kimler Kullanır?\n")

        # Encode path for markdown link
        # Relative path from base_dir to the lang_path
        rel_path = f"{category}/{lang_file}"
        url_encoded_path = urllib.parse.quote(rel_path)
        
        # Add to markdown content
        md_content += f"* [{lang}]({url_encoded_path})\n"
        
    md_content += "\n"

# Write README.md
readme_md_path = os.path.join(base_dir, "README.md")
with open(readme_md_path, 'w', encoding='utf-8') as f:
    f.write(md_content)

print(f"Sucessfully generated structure and updated {readme_md_path}")
