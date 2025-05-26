// File utilities for path normalization and file type detection
import * as path from 'path';
import * as fs from 'fs';

export interface FileInfo {
  absolutePath: string;
  exists: boolean;
  isReadable: boolean;
  extension: string;
  basename: string;
  dirname: string;
}

export class FileUtils {
  /**
   * Normalize and resolve file path to absolute path
   */
  static normalizePath(filePath: string): string {
    // Handle tilde expansion
    if (filePath.startsWith('~/')) {
      const homeDir = process.env.HOME || process.env.USERPROFILE || '';
      filePath = path.join(homeDir, filePath.slice(2));
    }
    
    // Resolve to absolute path
    return path.resolve(filePath);
  }

  /**
   * Get comprehensive file information
   */
  static async getFileInfo(filePath: string): Promise<FileInfo> {
    const absolutePath = this.normalizePath(filePath);
    
    let exists = false;
    let isReadable = false;
    
    try {
      await fs.promises.access(absolutePath, fs.constants.F_OK);
      exists = true;
      
      await fs.promises.access(absolutePath, fs.constants.R_OK);
      isReadable = true;
    } catch (error) {
      // File doesn't exist or isn't readable
    }
    
    return {
      absolutePath,
      exists,
      isReadable,
      extension: path.extname(absolutePath).toLowerCase(),
      basename: path.basename(absolutePath),
      dirname: path.dirname(absolutePath)
    };
  }

  /**
   * Check if file is a supported text file type
   */
  static isSupportedTextFile(filePath: string): boolean {
    const textExtensions = [
      '.js', '.ts', '.jsx', '.tsx', '.mjs', '.cjs',
      '.py', '.pyw', '.java', '.kt', '.scala',
      '.cpp', '.cc', '.cxx', '.c', '.h', '.hpp',
      '.cs', '.vb', '.fs', '.fsx',
      '.rb', '.php', '.go', '.rs', '.swift',
      '.md', '.markdown', '.txt', '.text',
      '.json', '.yaml', '.yml', '.toml', '.ini',
      '.xml', '.html', '.htm', '.css', '.scss', '.sass', '.less',
      '.sh', '.bash', '.zsh', '.fish', '.ps1', '.bat', '.cmd',
      '.sql', '.graphql', '.gql',
      '.el', '.lisp', '.clj', '.cljs', '.hs'
    ];
    
    const ext = path.extname(filePath).toLowerCase();
    return textExtensions.includes(ext);
  }

  /**
   * Get project-relative path if file is within project
   */
  static getProjectRelativePath(filePath: string, projectRoot: string): string | null {
    const absolutePath = this.normalizePath(filePath);
    const absoluteProjectRoot = this.normalizePath(projectRoot);
    
    if (absolutePath.startsWith(absoluteProjectRoot)) {
      return path.relative(absoluteProjectRoot, absolutePath);
    }
    
    return null;
  }
}
